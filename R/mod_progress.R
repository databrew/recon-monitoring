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
        column(4, pickerInput(ns("ward"), "Select Ward:", "",
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE,
                                             `live-search` = TRUE)),
               style="z-index:1002;"),
        column(4, pickerInput(ns("community_health_unit"), "Select CHU:", "",
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE,
                                             `live-search` = TRUE)),
               style="z-index:1002;")
      ),
      fluidRow(
        column(3, actionBttn(ns("submit"),
                             "Submit Selection",
                             color = "primary",
                             style = 'minimal'))

      ),
      br(),
      fluidRow(
        infoBoxOutput(ns("total_chv")),
        infoBoxOutput(ns("total_hh")),
        infoBoxOutput(ns("percent_hh"))

      ),
      br(),
      fluidRow(
        column(6, box(
          title = 'Map of Submissions',
          leafletOutput(ns('map_plot'), height = 400),
          width = NULL, solidHeader= TRUE, )),
        column(6, box(
          title = 'Cumulative Form Submissions by Day',
          plotlyOutput(ns('cumulative_submission'), height = 400),
          width = NULL, solidHeader= TRUE, ))
      ),
      fluidRow(
        column(6, box(
          title = 'Submission by Day',
          plotlyOutput(ns('submission_by_day'), height = 380),
          width = NULL, solidHeader= TRUE, )),
        column(6, box(
          title = 'Submission by Group',
          selectInput(ns("filter_group"), "View by:",
                      choices = c('ward', 'community_health_unit'),
                      selected = 'Ward'),
          plotlyOutput(ns('submission_by_filter'), height = 300),
          width = NULL, solidHeader= TRUE, ))

      )
    )
  )

}

# SERVER FOR MOST RECENT VALUE MAP
mod_progress_server <- function(input, output, session, data){

  hh <- get_household_forms()
  registration <- get_registartion_forms()

  chv_target <- registration %>%
    dplyr::filter(worker_type == 'CHV') %>%
    dplyr::group_by(wid) %>%
    dplyr::summarise(num_households = sum(num_households,na.rm=T))

  ward_list <- hh %>%
    .$ward %>%
    unique()

  community_health_unit_list <- hh %>%
    .$community_health_unit %>%
    unique()

  values <- reactiveValues(
    orig_data = hh,
    filter_data = hh,
    chv_target = chv_target,
    ward_list = ward_list,
    community_health_unit_list = community_health_unit_list
  )

  observe({
    updatePickerInput(session, "ward",
                      choices = sort(ward_list),
                      selected = ward_list)
    updatePickerInput(session, "community_health_unit",
                      choices = sort(community_health_unit_list),
                      selected = community_health_unit_list)
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
  # observeEvent(input$ward_chv, {
  #   values$ward_chv <- input$ward_chv
  # }, ignoreNULL = FALSE)

  observeEvent(input$submit, {
    values$filter_data <- values$orig_data %>%
      dplyr::filter(ward %in% input$ward) %>%
      dplyr::filter(community_health_unit %in% input$community_health_unit)
  }, ignoreNULL=FALSE)

  # ---- RENDER PLOT FROM REACTIVE DATA ---- #
  output$total_chv <- renderInfoBox({

    num_chv <- values$chv_target$wid %>%
      unique() %>%
      length()

    infoBox(
      "Total CHV",
      h2(num_chv),
      icon = icon("user"),
      color = 'navy',
      fill = TRUE
    )
  })

  output$total_hh <- renderInfoBox({
    if(input$submit == 0){
      data <- values$orig_data
    }else{
      data <- values$filter_data
    }

    total_hh <- data %>%
      .$hh_id %>%
      unique() %>%
      length()

    chv_target <- sum(values$chv_target$num_households)

    infoBox(
      "Household Forms Submitted by CHV",
      h2(total_hh),
      icon = icon("house"),
      color = 'navy',
      fill = TRUE
    )
  })

  output$percent_hh <- renderInfoBox({
    if(input$submit == 0){
      data <- values$orig_data
    }else{
      data <- values$filter_data
    }

    total_hh <- data %>%
      .$hh_id %>%
      unique() %>%
      length()

    chv_target <- sum(values$chv_target$num_households)

    perc <- total_hh/chv_target

    perc <- glue::glue(
      "{value}%",
      value = sprintf(((perc) * 100),
        fmt = '%#.1f'))


    title <- tags$div(
      glue::glue("% to Target "),
      glue::glue("({chv_target} Households)"))


    infoBox(
      title,
      h2(perc),
      icon = icon("percent"),
      color = 'navy',
      fill = TRUE
    )
  })


  output$map_plot <- renderLeaflet({

    data <- values$filter_data %>%
      dplyr::filter(!is.na(Longitude))

    content <- paste0(
    "
    <strong>Household ID</strong>: {hh_id}</br>
    <strong>Ward</strong>: {ward}</br>
    <strong>CHU</strong>: {community_health_unit}</br>
    ")

    if(input$submit == 0){
      data <- values$orig_data
    }else{
      data <- values$filter_data
    }

    data <- data %>%
      as_tibble(.name_repair = "universal") %>%
      dplyr::filter(!is.na(Latitude)) %>%
      dplyr::mutate(content = glue::glue(content))

    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        lng=~Longitude,
        stroke = FALSE,
        fillOpacity = 0.4,
        radius = 7,
        lat=~Latitude,
        popup=~content)
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
                    theme_minimal() +
                    labs(x = "", y = "") +
                    theme(axis.text.x = element_text(angle=45, hjust=1),
                          legend.position = "none") +
                    scale_fill_brewer(palette="Dark2")
                    )
    p
  })

}
