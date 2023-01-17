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
        column(3, actionBttn(ns("submit"),
                             "Submit Selection",
                             color = "primary",
                             style = 'unite'),
               style = 'margin-top:20px')
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
          title = 'Map of Household Submissions',
          footer = 'Household Form submission by each CHV, click to see more details',
          leafletOutput(ns('map_plot'), height = 400) %>% shinycssloaders::withSpinner(),
          width = NULL,
          solidHeader= TRUE)),
        column(6, box(
          title = 'Cumulative Form Submissions by Day',
          plotlyOutput(ns('cumulative_submission'), height = 400) %>% shinycssloaders::withSpinner(),
          width = NULL, solidHeader= TRUE, ))
      ),
      fluidRow(
        column(6, box(
          title = 'Submission by Day',
          plotlyOutput(ns('submission_by_day'), height = 380) %>% shinycssloaders::withSpinner(),
          width = NULL, solidHeader= TRUE, )),
        column(6, box(
          title = 'Submission by Group',
          selectInput(ns("filter_group"), "View by:",
                      choices = c('ward', 'community_health_unit'),
                      selected = 'Ward'),
          plotlyOutput(ns('submission_by_filter'), height = 300) %>% shinycssloaders::withSpinner(),
          width = NULL, solidHeader= TRUE, ))

      )
    )
  )

}

# SERVER FOR MOST RECENT VALUE MAP
mod_progress_server <- function(input, output, session, data){
  # get household and registration forms
  hh <- get_household_forms()
  registration <- get_registration_forms()

  # get chv targets based on registration forms
  chv_target <- registration %>%
    dplyr::filter(
      worker_type == 'CHV',
      ward_chv %in% hh$ward,
      community_health_unit_chv %in%
        hh$community_health_unit) %>%
    dplyr::group_by(wid, ward_chv, community_health_unit_chv) %>%
    dplyr::summarise(num_households = sum(num_households,na.rm=T)) %>%
    dplyr::ungroup()

  # get ward lists
  ward_list <- c(hh$ward, chv_target$ward_chv) %>% unique()

  # get community health unit
  community_health_unit_list <- c(hh$community_health_unit, chv_target$community_health_unit_chv) %>% unique()

  # instantiate reactive datasets
  orig_hh_data <- reactive({hh})
  orig_chv_target <- reactive({chv_target})

  # instantate reactive values for lists
  values <- reactiveValues(
    ward_list = ward_list,
    community_health_unit_list = community_health_unit_list
  )

  # update picker input based on lists
  observe({
    updatePickerInput(session, "ward",
                      choices = sort(values$ward_list),
                      selected = values$ward_list)
    updatePickerInput(session, "community_health_unit",
                      choices = sort(values$community_health_unit_list),
                      selected = values$community_health_unit_list)
  })

  # observe ward selection for dependency filters
  observeEvent(input$ward, {
    f <- orig_hh_data() %>%
      dplyr::filter(ward %in% input$ward) %>%
      .$community_health_unit %>%
      unique()
    updatePickerInput(session, "community_health_unit",
                      choices = sort(f),
                      selected = f)
  }, ignoreNULL = FALSE)

  # filter CHVs on household data based on wards and chu
  filter_hh_data <- eventReactive(input$submit, {
    orig_hh_data() %>%
      dplyr::filter(ward %in% input$ward) %>%
      dplyr::filter(
        community_health_unit %in%
          input$community_health_unit)
  }, ignoreNULL=FALSE)

  # filter CHV targets dynamically based on inputs
  filter_chv_target <- eventReactive(input$submit, {
    orig_chv_target() %>%
      dplyr::filter(ward_chv %in% input$ward) %>%
      dplyr::filter(community_health_unit_chv %in% input$community_health_unit)
  }, ignoreNULL=FALSE)


  # ---- RENDER PLOT FROM REACTIVE DATA ---- #
  output$total_chv <- renderInfoBox({
    if(input$submit == 0){
      chv_data <- orig_chv_target()
      hh_data <- orig_hh_data()
    }else{
      chv_data <- filter_chv_target()
      hh_data <- filter_hh_data()
    }
    num_chv <- hh_data$wid %>%
      unique() %>%
      length()

    infoBox(
      "Number CHV with Submitted Forms",
      h3(num_chv),
      icon = icon("user"),
      color = 'black',
      width = 6
    )
  })

  output$total_hh <- renderInfoBox({
    if(input$submit == 0){
      data <- orig_hh_data()
    }else{
      data <- filter_hh_data()
    }

    total_hh <- data$hh_id %>%
      unique() %>%
      length()

    infoBox(
      "Number of Household Forms Submitted",
      h3(total_hh),
      icon = icon("house"),
      color = 'black',
      width = 6
    )
  })

  output$percent_hh <- renderInfoBox({
    if(input$submit == 0){
      chv_data <- orig_chv_target()
      hh_data <- orig_hh_data()
    }else{
      chv_data <- filter_chv_target()
      hh_data <- filter_hh_data()
    }
    total_hh <- hh_data %>%
      .$hh_id %>%
      unique() %>%
      length()

    chv_target <- sum(chv_data$num_households)

    perc <- total_hh/chv_target

    perc <- glue::glue(
      "{value}% ({total_hh}/{chv_target})",
      value = sprintf(((perc) * 100),
        fmt = '%#.1f'))


    title <- tags$div(
      glue::glue("% to Recon Target"))


    infoBox(
      title,
      h3(perc),
      icon = icon("percent"),
      color = 'black',
      width = 6
    )
  })

  # map plots
  output$map_plot <- renderLeaflet({
    if(input$submit == 0){
      data <- orig_hh_data()
    }else{
      data <- filter_hh_data()
    }

    content <- paste0(
      "
    <strong>Household ID</strong>: {hh_id}</br>
    <strong>Ward</strong>: {ward}</br>
    <strong>CHU</strong>: {community_health_unit}</br>
    <strong>CHV</strong>: {wid}</br>
    ")


    data <- data %>%
      as_tibble(.name_repair = "universal") %>%
      dplyr::filter(!is.na(Latitude)) %>%
      dplyr::mutate(content = glue::glue(content))

    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        lng=~Longitude,
        color = "darkblue",
        stroke = FALSE,
        fillOpacity = 0.5,
        radius = 3,
        lat=~Latitude,
        popup=~content)
  })

  output$cumulative_submission <- renderPlotly({

    if(input$submit == 0){
      data <- orig_hh_data()
    }else{
      data <- filter_hh_data()
    }
    summary <- data %>%
      tibble::as_tibble(.name_repair = "unique") %>%
      dplyr::mutate(day = lubridate::as_date(SubmissionDate)) %>%
      dplyr::group_by(day) %>%
      dplyr::summarise(n = n()) %>%
      dplyr::mutate(n = rollapplyr(n, FUN = sum, width = nrow(.), partial = TRUE))
    p <- ggplotly(summary %>%
                    ggplot2::ggplot(aes(x = day, y = n)) +
                    geom_line(color = 'darkblue') +
                    geom_point() +
                    theme_minimal() +
                    theme(panel.grid.major.x = element_blank() ,
                          legend.position = "none") +
                    labs(x = "", y = ""))
    p
  })


  output$submission_by_day <- renderPlotly({
    if(input$submit == 0){
      data <- orig_hh_data()
    }else{
      data <- filter_hh_data()
    }
    summary <- data %>%
      dplyr::group_by(day_of_week = lubridate::wday(
        SubmissionDate,
        label = TRUE, abbr = FALSE)) %>%
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
                    geom_bar(stat = 'identity', fill = 'dodgerblue4') +
                    theme_minimal() +
                    labs(x = "", y = "") +
                    theme(axis.text.x = element_text(hjust=1),
                          panel.grid.major.x = element_blank() ,
                          legend.position = "none")
                    )
    p
  })

  output$submission_by_filter <- renderPlotly({
    if(input$submit == 0){
      data <- orig_hh_data()
    }else{
      data <- filter_hh_data()
    }
    summary <- data %>%
      tibble::as_tibble(.name_repair = "unique") %>%
      dplyr::group_by(!!sym(input$filter_group)) %>%
      dplyr::summarise(n = n())
    p <- ggplotly(summary %>%
                    ggplot2::ggplot(aes_string(y = input$filter_group,
                                               x = "n",
                                               fill = input$filter_group)) +
                    geom_bar(stat = 'identity') +
                    theme_minimal() +
                    labs(x = "", y = "") +
                    theme(axis.text.y = element_text(hjust=1),
                          panel.grid.major.y = element_blank(),
                          legend.position = "none") +
                    scale_fill_brewer(palette="Dark2")
                    )
    p
  })

}
