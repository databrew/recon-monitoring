# Module progress
#' @title mod_progress.R
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
# UI FOR MOST RECENT VALUE RADAR PLOT
mod_progress_ui <- function(id){
  # let leaflet know that selections should persist
  # options(persistent = TRUE)
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(3, pickerInput(ns("ward"), "Select Ward:", "",
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE,
                                             `live-search` = TRUE)),
               style="z-index:1002;"),
        column(3, pickerInput(ns("community_health_unit"), "Select CHU:", "",
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE,
                                             `live-search` = TRUE)),
               style="z-index:1002;"),
        column(3, pickerInput(ns("village"), "Select Village:", "",
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE,
                                             `live-search` = TRUE)),
               style="z-index:1002;"),
        column(1, actionBttn(ns("submit"),
                             "Submit Selection",
                             color = "primary",
                             style = 'simple'))
      ),
      br(),
      fluidRow(
        column(6, box(
          title = 'Map of Submissions',
          leafletOutput(ns('map_plot'), height = 400),
          width = NULL, status = "primary", solidHeader = TRUE)),
        column(6, box(
          title = 'Cumulative Submissions',
          plotlyOutput(ns('cumulative_submission'), height = 400),
          width = NULL, status = "primary", solidHeader = TRUE))
      ),
      fluidRow(
        column(6, box(
          title = 'Submission by Day',
          plotlyOutput(ns('submission_by_day'), height = 380),
          width = NULL, status = "primary", solidHeader = TRUE)),
        column(6, box(
          title = 'Submission by Group',
          selectInput(ns("filter_group"), "View by:",
                      choices = c('ward', 'community_health_unit', 'village'),
                      selected = 'Ward'),
          plotlyOutput(ns('submission_by_filter'), height = 300),
          width = NULL, status = "primary", solidHeader = TRUE))

      )
    )
  )

}

# SERVER FOR MOST RECENT VALUE MAP
mod_progress_server <- function(input, output, session){
  filename <- tempfile(fileext = '.csv')
  data <-  get_s3_data(s3obj = paws::s3(),
                       bucket = 'databrew.org',
                       object_key = "kwale/clean-form/reconbhouseholdtraining/reconbhouseholdtraining.csv",
                       filename = filename) %>%
    read.csv(.) %>%
    tibble::as_tibble(.name_repair = "unique") %>%
    dplyr::mutate(ward = ifelse(ward == "", "N/A", ward),
                  community_health_unit = ifelse(community_health_unit == "", "N/A", community_health_unit),
                  village = ifelse(village == "", "N/A", village))
  ward_list <- data %>%
    .$ward %>%
    unique()

  community_health_unit_list <- data %>%
    .$community_health_unit %>%
    unique()

  village_list <- data %>%
    .$village %>%
    unique()


  values <- reactiveValues(
    orig_data = data,
    filter_data = data,
    ward_list = ward_list,
    community_health_unit_list = community_health_unit_list,
    village_list = village_list
  )

  observe({
    updatePickerInput(session, "ward",
                      choices = sort(ward_list),
                      selected = ward_list)
    updatePickerInput(session, "community_health_unit",
                      choices = sort(community_health_unit_list),
                      selected = community_health_unit_list)
    updatePickerInput(session, "village",
                      choices = sort(village_list),
                      selected = village_list)
  })

  observeEvent(input$ward, {
    f <- values$orig_data %>%
      dplyr::filter(ward %in% input$ward) %>%
      .$community_health_unit %>%
      unique()
    updatePickerInput(session, "community_health_unit",
                      choices = sort(f),
                      selected = f)
  }, ignoreNULL = FALSE)

  observeEvent(input$community_health_unit, {
    f <- values$orig_data %>%
      dplyr::filter((ward %in% input$ward) &
                      (community_health_unit %in% input$community_health_unit)) %>%
      .$village %>%
      unique()
    updatePickerInput(session, "village",
                      choices = sort(f),
                      selected = f)
  }, ignoreNULL = FALSE)

  # observeEvent(input$ward_chv, {
  #   values$ward_chv <- input$ward_chv
  # }, ignoreNULL = FALSE)

  observeEvent(input$submit, {
    values$filter_data <- values$orig_data %>%
      dplyr::filter(ward %in% input$ward) %>%
      dplyr::filter(community_health_unit %in% input$community_health_unit) %>%
      dplyr::filter(village %in% input$village)
  }, ignoreNULL=FALSE)

  # ---- RENDER PLOT FROM REACTIVE DATA ---- #
  output$map_plot <- renderLeaflet({

    data <- values$filter_data %>%
      dplyr::filter(!is.na(Longitude))

    label <- glue::glue(
    "<strong>Ward</strong>: {data$ward}</br>
    <strong>CHU</strong>: {data$community_health_unit}</br>
    <strong>Village</strong>: {data$village}") %>% lapply(htmltools::HTML)

    if(input$submit == 0){
      data <- values$orig_data
    }else{
      data <- values$filter_data
    }

    data <- data %>%
      as_tibble(.name_repair = "universal") %>%
      dplyr::filter(!is.na(Latitude)) %>%
      dplyr::select(ward, Latitude, Longitude ,community_health_unit, village)

    if(nrow(data) > 0){
      mapv <- mapview(
        data,
        xcol = "Longitude",
        ycol="Latitude",
        crs = 4269,
        grid = FALSE,
        label = label)

      map_plot <- mapv + mapview(NULL,
                                 crs = 4269,
                                 grid = FALSE)

      map_plot@map

    }else{
      mapview(NULL,
              crs = 4269,
              grid = FALSE)
    }
  })

  output$cumulative_submission <- renderPlotly({

    if(input$submit == 0){
      data <- values$orig_data
    }else{
      data <- values$filter_data
    }
    summary <- data %>%
      tibble::as_tibble(.name_repair = "unique") %>%
      dplyr::mutate(day = lubridate::as_date(SubmissionDate)) %>%
      dplyr::group_by(day) %>%
      dplyr::summarise(n = n())
    p <- ggplotly(summary %>%
                    ggplot2::ggplot(aes(x = day, y = n)) +
                    geom_line(color = 'darkblue') +
                    geom_point() +
                    scale_y_continuous(breaks=seq(from = 0 , to = round(max(summary$n)), by = 1)) +
                    theme_minimal() +
                    labs(x = "", y = ""))
    p
  })


  output$submission_by_day <- renderPlotly({
    if(input$submit == 0){
      data <- values$orig_data
    }else{
      data <- values$filter_data
    }
    summary <- data %>%
      dplyr::group_by(day_of_week = lubridate::wday(SubmissionDate, label = TRUE, abbr = FALSE)) %>%
      dplyr::group_by(day_of_week) %>%
      dplyr::summarise(n = n())


    p <- ggplotly(tibble(day_of_week = c('Monday',
                                         'Tuesday',
                                         'Wednesday',
                                         'Thursday',
                                         'Friday',
                                         'Saturday',
                                         'Sunday')) %>%
                    dplyr::left_join(summary) %>%
                    dplyr::mutate(n = ifelse(is.na(n), 0, n),
                                  ord = row_number(),
                                  day_of_week = fct_reorder(day_of_week, ord)) %>%
                    ggplot(aes(x = day_of_week, y = n)) +
                    geom_bar(stat = 'identity', alpha = 0.9, fill = 'dodgerblue4') +
                    scale_y_continuous(breaks=seq(from = 0 , to = round(max(summary$n)), by = 1)) +
                    theme_minimal() +
                    labs(x = "", y = "") +
                    theme(axis.text.x = element_text(angle=45, hjust=1),
                          legend.position = "none")
                    )
    p
  })

  output$submission_by_filter <- renderPlotly({
    if(input$submit == 0){
      data <- values$orig_data
    }else{
      data <- values$filter_data
    }
    summary <- data %>%
      tibble::as_tibble(.name_repair = "unique") %>%
      dplyr::group_by(!!sym(input$filter_group)) %>%
      dplyr::summarise(n = n())
    p <- ggplotly(summary %>%
                    ggplot2::ggplot(aes_string(x = input$filter_group,
                                               y = "n",
                                               fill = input$filter_group)) +
                    geom_bar(stat = 'identity', alpha = 0.9) +
                    scale_y_continuous(breaks=seq(from = 0 , to = round(max(summary$n)), by = 1)) +
                    theme_minimal() +
                    labs(x = "", y = "") +
                    theme(axis.text.x = element_text(angle=45, hjust=1),
                          legend.position = "none") +
                    scale_fill_brewer(palette="Accent")
                    )
    p
  })

}
