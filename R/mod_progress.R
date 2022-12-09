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
          leafletOutput(ns('map_plot'), height = 400),
          width = NULL,
          solidHeader= TRUE)),
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
  registration <- get_registration_forms()

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
    orig_chv_target = chv_target,
    filter_chv_target = chv_target,
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
  waiter_hide()

  observeEvent(input$ward, {
    f <- values$orig_data %>%
      dplyr::filter(ward %in% input$ward) %>%
      .$community_health_unit %>%
      unique()
    updatePickerInput(session, "community_health_unit",
                      choices = sort(f),
                      selected = f)
  }, ignoreNULL = FALSE)

  observeEvent(input$submit, {
    values$filter_data <- values$orig_data %>%
      dplyr::filter(ward %in% input$ward) %>%
      dplyr::filter(
        community_health_unit %in%
          input$community_health_unit)
    values$filter_chv_target <- values$orig_chv_target
  }, ignoreNULL=FALSE)


  # ---- RENDER PLOT FROM REACTIVE DATA ---- #
  output$total_chv <- renderInfoBox({
    if(input$submit == 0){
      data <- values$orig_chv_target
    }else{
      data <- values$filter_chv_target
    }
    num_chv <- data$wid %>%
      unique() %>%
      length()

    infoBox(
      "Total CHV",
      h3(num_chv),
      icon = icon("user"),
      color = 'black',
      width = 6
    )
  })

  output$total_hh <- renderInfoBox({
    if(input$submit == 0){
      data <- values$orig_data
    }else{
      data <- values$filter_data
    }

    total_hh <- data$hh_id %>%
      unique() %>%
      length()

    infoBox(
      "Household Forms Submitted by CHV",
      h3(total_hh),
      icon = icon("house"),
      color = 'black',
      width = 6
    )
  })

  output$percent_hh <- renderInfoBox({
    if(input$submit == 0){
      chv_data <- values$orig_chv_target
      hh_data <- values$orig_data
    }else{
      chv_data <- values$filter_chv_target
      hh_data <- values$filter_data
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
      glue::glue("% to Household Recon Target"))


    infoBox(
      title,
      h3(perc),
      icon = icon("percent"),
      color = 'black',
      width = 6
    )
  })


  output$map_plot <- renderLeaflet({
    if(input$submit == 0){
      data <- values$orig_data
    }else{
      data <- values$filter_data
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
  }) %>% bindCache(values$orig_data, values$filter_data, cache = cachem::cache_disk("./myapp-cache"))

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
                    theme(panel.grid.major.x = element_blank() ,
                          legend.position = "none") +
                    labs(x = "", y = ""))
    p
  }) %>% bindCache(values$orig_data, values$filter_data, cache = cachem::cache_disk("./myapp-cache"))


  output$submission_by_day <- renderPlotly({
    if(input$submit == 0){
      data <- values$orig_data
    }else{
      data <- values$filter_data
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
                    geom_bar(stat = 'identity', alpha = 0.9, fill = 'dodgerblue4') +
                    theme_minimal() +
                    labs(x = "", y = "") +
                    theme(axis.text.x = element_text(angle=45, hjust=1),
                          panel.grid.major.x = element_blank() ,
                          legend.position = "none")
                    )
    p
  }) %>% bindCache(values$orig_data, values$filter_data, cache = cachem::cache_disk("./myapp-cache"))

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
                          panel.grid.major.x = element_blank(),
                          legend.position = "none") +
                    scale_fill_brewer(palette="Dark2")
                    )
    p
  }) %>% bindCache(values$orig_data, values$filter_data, cache = cachem::cache_disk("./myapp-cache"))

}
