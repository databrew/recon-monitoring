#' fieldworker_performance UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fieldworker_performance_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(3, pickerInput(
          ns("cha_wid"), "Select CHA ID:", "",
          multiple = TRUE,
          options = list(`actions-box` = TRUE,
          `live-search` = TRUE)),
          style="z-index:1002;"),
        column(3, pickerInput(
          ns("chv_wid"), "Select CHV ID:", "",
          multiple = TRUE,
          options = list(`actions-box` = TRUE,
                         `live-search` = TRUE)),
          style="z-index:1002;"),
        column(3, actionBttn(
          ns("submit"),
          "Submit Selection",
          color = "primary",
          style = 'unite'),
          style = 'margin-top:20px')
      ),
      br(),
      fluidRow(
        column(6, box(
          title = 'Submissions Map',
          footer = 'Household Form submission by each CHV under CHA, click to see more details',
          leafletOutput(ns('cha_map_plot'), height = 400),
          width = NULL, solidHeader= TRUE)),
        column(6, box(
          title = 'Fieldworker Summary Table',
          tags$button(
            tagList(icon("download"), "Download"),
            onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')",
                              ns('cha_chv_summary_table'),
                              "summary_performance_cha.csv")
          ),
          reactableOutput(ns('cha_chv_summary_table'), height = 375),
          width = NULL, solidHeader= TRUE, ))
      ),
      fluidRow(
        column(6, box(
          title = 'CHA Raw Table',
          tags$button(
            tagList(icon("download"), "Download"),
            onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')",
                              ns('cha_table'),
                              "cha.csv")
          ),
          reactableOutput(ns('cha_table'), height = 400),
          width = NULL, solidHeader= TRUE, )),
        column(6, box(
          title = 'CHV Raw Table',
          tags$button(
            tagList(icon("download"), "Download"),
            onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')",
                              ns('chv_table'),
                              "chv.csv")
          ),
          reactableOutput(ns('chv_table'), height = 400),
          width = NULL, solidHeader= TRUE, ))
        )
      )
  )
}

#' fieldworker_performance Server Functions
#'
#' @noRd
mod_fieldworker_performance_server <- function(input, output, session){
  ns <- session$ns

  registration <-  get_registration_forms()
  hh <- get_household_forms() %>%
    dplyr::select(-end_time)

  cha_data <- registration %>%
    dplyr::filter(worker_type == 'CHA') %>%
    dplyr::select(wid,
                  community_health_unit = community_health_unit_chv,
                  username,
                  subcounty = sub_county_cha,
                  ward = ward_cha,
                  num_households,
                  internet_conn_cha,
                  number_chv_supervise)

  chv_data <- registration %>%
    dplyr::filter(worker_type == 'CHV') %>%
    dplyr::select(wid,
                  wid_cha,
                  community_health_unit = community_health_unit_chv,
                  username,
                  villages = villages_chv,
                  subcounty = sub_county_chv,
                  ward = ward_chv,
                  num_households,
                  internet_conn_chv)


  #############################
  # table 2: data about CHV
  #############################
  # specs:
  # - name
  # - subcounty
  # - ward
  # - CHU
  # - villages
  # - Target number of households
  # - number of household forms submitted
  # - number of days with household forms submitted
  # - name/ID of CHA
  chv_data_summarised <- chv_data %>%
    dplyr::left_join(hh %>%
                       dplyr::group_by(wid) %>%
                       dplyr::summarise(
                         num_household_form_submitted = n_distinct(hh_id)),
                     by = "wid") %>%
    dplyr::distinct(wid, .keep_all = TRUE) %>%
    dplyr::mutate(
      num_household_form_submitted = ifelse(
        is.na(num_household_form_submitted),
        0, num_household_form_submitted),
      household_form_percent_completion = glue::glue(
        "{value}%",
        value = sprintf(((
          num_household_form_submitted / num_households) * 100),
          fmt = '%#.1f'))
    )

  #############################
  # table 1: data about CHA
  #############################
  # specs:
  # - name
  # - subcounty
  # - ward
  # - CHU
  # - number of CHVs supervised
  # - names/IDs of CHVs supervised
  # - number of households overseen
  # - reported internet connectivity
  # - number of forms submitted by CHVs supervised
  cha_data_summarised <- cha_data %>%
    dplyr::select(-num_households) %>%
    dplyr::left_join(chv_data_summarised %>%
                       dplyr::select(wid_chv = wid,
                                     wid_cha,
                                     num_household_form_submitted,
                                     num_households,
                                     internet_conn_chv),
                     by = c("wid" = "wid_cha")) %>%
    dplyr::mutate(internet_conn = coalesce(
      internet_conn_cha, internet_conn_chv))


  chv_data_summarised %>%
    dplyr::select(wid_chv = wid,
                  wid_cha,
                  num_household_form_submitted,
                  num_households,
                  internet_conn_chv) %>%
    dplyr::left_join(cha_data %>% select(-num_households))


  ##################################
  # Get list
  ##################################
  cha_list <- cha_data %>%
    .$wid %>%
    unique()

  chv_list <- chv_data %>%
    .$wid %>%
    unique()

  ##################################
  # Data for maps
  ##################################
  map_data <- hh %>%
    dplyr::select(wid,
                  hh_id,
                  Longitude,
                  Latitude,
                  ward,
                  village,
                  community_health_unit) %>%
    dplyr::inner_join(chv_data %>%
                       dplyr::select(wid, wid_cha) %>%
                       distinct(), by = "wid") %>%
    dplyr::mutate(wid_cha = factor(wid_cha)) %>%
    dplyr::filter(!is.na(Latitude),
                  !is.na(Longitude),
                  wid_cha %in% cha_list)



  values <- reactiveValues(
    filter_cha_chv_data = cha_data_summarised,
    orig_cha_data = cha_data_summarised,
    filter_cha_data = cha_data_summarised,
    orig_chv_data = chv_data_summarised,
    filter_chv_data = chv_data_summarised,
    orig_map_data = map_data,
    filter_map_data = map_data,
    cha_list = cha_list,
    chv_list = chv_list
  )

  observe({
    updatePickerInput(session, "cha_wid",
                      choices = sort(values$cha_list),
                      selected = values$cha_list)
    updatePickerInput(session, "chv_wid",
                      choices = sort(values$chv_list),
                      selected = values$chv_list)
  })

  observeEvent(input$cha_wid, {
    f <- values$orig_chv_data %>%
      dplyr::filter(wid_cha %in% input$cha_wid) %>%
      .$wid %>%
      unique()
    updatePickerInput(session, "chv_wid",
                      choices = sort(f),
                      selected = f)
  }, ignoreNULL = FALSE)

  observeEvent(input$submit, {
    # values$filter_cha_data <- values$orig_cha_data %>%
    #   dplyr::filter(wid %in% input$cha_wid)
    values$filter_cha_data <- values$orig_cha_data %>%
      dplyr::filter(wid %in% input$cha_wid)

    values$filter_chv_data <- values$orig_chv_data %>%
      dplyr::filter(wid %in% input$chv_wid)

    values$filter_cha_chv_data <- values$orig_cha_data %>%
      dplyr::filter(wid %in% input$cha_wid) %>%
      dplyr::filter(wid_chv %in% input$chv_wid)

    values$filter_map_data <- values$orig_map_data %>%
      dplyr::filter(wid_cha %in% input$cha_wid) %>%
      dplyr::filter(wid %in% input$chv_wid)

  }, ignoreNULL=FALSE)


  output$cha_chv_summary_table = renderReactable({
    if(input$submit == 0){
      data <- values$orig_cha_data
    }else{
      data <- values$filter_cha_chv_data
    }

    js_func <- JS("function(values, rows) {
        let totalSubmitted = 0
        let totalTarget = 0
        rows.forEach(function(row) {
          totalTarget += row['num_households']
          totalSubmitted += row['num_household_form_submitted']
        })
        return totalSubmitted / totalTarget
      }")
    reactable(
      data = data %>%
        dplyr::select(
          wid_cha = wid,
          wid_chv,
          num_household_form_submitted,
          num_households) %>%
        dplyr::mutate(percent_completion =
                        num_household_form_submitted/num_households),
      groupBy = c("wid_cha"),
      columns = list(
        num_household_form_submitted = colDef(aggregate = "sum"),
        num_households = colDef(aggregate = "sum"),
        percent_completion = colDef(aggregate = js_func,
                                    format = colFormat(
                                      percent = TRUE,
                                      digits = 1))
      ),
      defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
    )
  })

  output$cha_table = renderReactable({
    if(input$submit == 0){
      data <- values$orig_cha_data
    }else{
      data <- values$filter_cha_data
    }
    reactable(data %>%
                dplyr::select(
                  wid,
                  subcounty,
                  ward,
                  community_health_unit,
                  internet_conn_cha,
                  number_chv_supervise
                ) %>%
                distinct(wid,.keep_all = TRUE))
  })


  output$chv_table = renderReactable({
    if(input$submit == 0){
      data <- values$orig_chv_data
    }else{
      data <- values$filter_chv_data
    }
    reactable(data)
  })


  output$cha_map_plot <- renderLeaflet({
    if(input$submit == 0){
      data <- values$orig_map_data
    }else{
      data <- values$filter_map_data
    }

    content_placeholder <- paste0("Household ID: {hh_id}<br/>",
                                  "Ward: {ward}<br/>",
                                  "Village: {village}<br/>",
                                  "CHA ID: {wid_cha}<br/>",
                                  "CHV ID: {wid}")

    data <- data  %>%
      dplyr::mutate(content = glue::glue(content_placeholder))

    pal <- colorFactor(palette = "Dark2",
                        domain = unique(data$wid_cha))

    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        lng=~Longitude,
        stroke = FALSE,
        fillOpacity = 0.5,
        radius = 3,
        lat=~Latitude,
        popup=~content,
        color = ~pal(wid_cha))
  })

}

## To be copied in the UI
# mod_fieldworker_performance_ui("fieldworker_performance_1")

## To be copied in the server
# mod_fieldworker_performance_server("fieldworker_performance_1")
#' fieldworker_performance UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fieldworker_performance_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(3, pickerInput(
          ns("cha_wid"), "Select CHA ID:", "",
          multiple = TRUE,
          options = list(`actions-box` = TRUE,
          `live-search` = TRUE)),
          style="z-index:1002;"),
        column(3, pickerInput(
          ns("chv_wid"), "Select CHV ID:", "",
          multiple = TRUE,
          options = list(`actions-box` = TRUE,
                         `live-search` = TRUE)),
          style="z-index:1002;"),
        column(3, actionBttn(
          ns("submit"),
          "Submit Selection",
          color = "primary",
          style = 'unite'),
          style = 'margin-top:20px')
      ),
      br(),
      fluidRow(
        column(6, box(
          title = 'Submissions Map',
          footer = 'Household Form submission by each CHV under CHA, click to see more details',
          leafletOutput(ns('cha_map_plot'), height = 400),
          width = NULL, solidHeader= TRUE)),
        column(6, box(
          title = 'Fieldworker Summary Table',
          tags$button(
            tagList(icon("download"), "Download"),
            onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')",
                              ns('cha_chv_summary_table'),
                              "summary_performance_cha.csv")
          ),
          reactableOutput(ns('cha_chv_summary_table'), height = 375),
          width = NULL, solidHeader= TRUE, ))
      ),
      fluidRow(
        column(6, box(
          title = 'CHA Raw Table',
          tags$button(
            tagList(icon("download"), "Download"),
            onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')",
                              ns('cha_table'),
                              "cha.csv")
          ),
          reactableOutput(ns('cha_table'), height = 400),
          width = NULL, solidHeader= TRUE, )),
        column(6, box(
          title = 'CHV Raw Table',
          tags$button(
            tagList(icon("download"), "Download"),
            onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')",
                              ns('chv_table'),
                              "chv.csv")
          ),
          reactableOutput(ns('chv_table'), height = 400),
          width = NULL, solidHeader= TRUE, ))
        )
      )
  )
}

#' fieldworker_performance Server Functions
#'
#' @noRd
mod_fieldworker_performance_server <- function(input, output, session){
  ns <- session$ns

  registration <-  get_registration_forms()
  hh <- get_household_forms() %>%
    dplyr::select(-end_time)

  cha_data <- registration %>%
    dplyr::filter(worker_type == 'CHA') %>%
    dplyr::select(wid,
                  community_health_unit = community_health_unit_chv,
                  username,
                  subcounty = sub_county_cha,
                  ward = ward_cha,
                  num_households,
                  internet_conn_cha,
                  number_chv_supervise)

  chv_data <- registration %>%
    dplyr::filter(worker_type == 'CHV') %>%
    dplyr::select(wid,
                  wid_cha,
                  community_health_unit = community_health_unit_chv,
                  username,
                  villages = villages_chv,
                  subcounty = sub_county_chv,
                  ward = ward_chv,
                  num_households,
                  internet_conn_chv)


  #############################
  # table 2: data about CHV
  #############################
  # specs:
  # - name
  # - subcounty
  # - ward
  # - CHU
  # - villages
  # - Target number of households
  # - number of household forms submitted
  # - number of days with household forms submitted
  # - name/ID of CHA
  chv_data_summarised <- chv_data %>%
    dplyr::left_join(hh %>%
                       dplyr::group_by(wid) %>%
                       dplyr::summarise(
                         num_household_form_submitted = n_distinct(hh_id)),
                     by = "wid") %>%
    dplyr::distinct(wid, .keep_all = TRUE) %>%
    dplyr::mutate(
      num_household_form_submitted = ifelse(
        is.na(num_household_form_submitted),
        0, num_household_form_submitted),
      household_form_percent_completion = glue::glue(
        "{value}%",
        value = sprintf(((
          num_household_form_submitted / num_households) * 100),
          fmt = '%#.1f'))
    )

  #############################
  # table 1: data about CHA
  #############################
  # specs:
  # - name
  # - subcounty
  # - ward
  # - CHU
  # - number of CHVs supervised
  # - names/IDs of CHVs supervised
  # - number of households overseen
  # - reported internet connectivity
  # - number of forms submitted by CHVs supervised
  cha_data_summarised <- cha_data %>%
    dplyr::select(-num_households) %>%
    dplyr::left_join(chv_data_summarised %>%
                       dplyr::select(wid_chv = wid,
                                     wid_cha,
                                     num_household_form_submitted,
                                     num_households,
                                     internet_conn_chv),
                     by = c("wid" = "wid_cha")) %>%
    dplyr::mutate(internet_conn = coalesce(
      internet_conn_cha, internet_conn_chv))


  ##################################
  # Get list
  ##################################
  cha_list <- cha_data %>%
    .$wid %>%
    unique()

  chv_list <- chv_data %>%
    .$wid %>%
    unique()

  ##################################
  # Data for maps
  ##################################
  map_data <- hh %>%
    dplyr::select(wid,
                  hh_id,
                  Longitude,
                  Latitude,
                  ward,
                  village,
                  community_health_unit) %>%
    dplyr::inner_join(chv_data %>%
                       dplyr::select(wid, wid_cha) %>%
                       distinct(), by = "wid") %>%
    dplyr::mutate(wid_cha = factor(wid_cha)) %>%
    dplyr::filter(!is.na(Latitude),
                  !is.na(Longitude),
                  wid_cha %in% cha_list)



  values <- reactiveValues(
    filter_cha_chv_data = cha_data_summarised,
    orig_cha_data = cha_data_summarised,
    filter_cha_data = cha_data_summarised,
    orig_chv_data = chv_data_summarised,
    filter_chv_data = chv_data_summarised,
    orig_map_data = map_data,
    filter_map_data = map_data,
    cha_list = cha_list,
    chv_list = chv_list
  )

  observe({
    updatePickerInput(session, "cha_wid",
                      choices = sort(values$cha_list),
                      selected = values$cha_list)
    updatePickerInput(session, "chv_wid",
                      choices = sort(values$chv_list),
                      selected = values$chv_list)
  })

  observeEvent(input$cha_wid, {
    f <- values$orig_chv_data %>%
      dplyr::filter(wid_cha %in% input$cha_wid) %>%
      .$wid %>%
      unique()
    updatePickerInput(session, "chv_wid",
                      choices = sort(f),
                      selected = f)
  }, ignoreNULL = FALSE)

  observeEvent(input$submit, {
    # values$filter_cha_data <- values$orig_cha_data %>%
    #   dplyr::filter(wid %in% input$cha_wid)
    values$filter_cha_data <- values$orig_cha_data %>%
      dplyr::filter(wid %in% input$cha_wid)

    values$filter_chv_data <- values$orig_chv_data %>%
      dplyr::filter(wid %in% input$chv_wid)

    values$filter_cha_chv_data <- values$orig_cha_data %>%
      dplyr::filter(wid %in% input$cha_wid) %>%
      dplyr::filter(wid_chv %in% input$chv_wid)

    values$filter_map_data <- values$orig_map_data %>%
      dplyr::filter(wid_cha %in% input$cha_wid) %>%
      dplyr::filter(wid %in% input$chv_wid)

  }, ignoreNULL=FALSE)


  output$cha_chv_summary_table = renderReactable({
    if(input$submit == 0){
      data <- values$orig_cha_data
    }else{
      data <- values$filter_cha_chv_data
    }

    js_func <- JS("function(values, rows) {
        let totalSubmitted = 0
        let totalTarget = 0
        rows.forEach(function(row) {
          totalTarget += row['num_households']
          totalSubmitted += row['num_household_form_submitted']
        })
        return totalSubmitted / totalTarget
      }")
    reactable(
      data = data %>%
        dplyr::select(
          wid_cha = wid,
          wid_chv,
          num_household_form_submitted,
          num_households) %>%
        dplyr::mutate(percent_completion =
                        num_household_form_submitted/num_households),
      groupBy = c("wid_cha"),
      columns = list(
        num_household_form_submitted = colDef(aggregate = "sum"),
        num_households = colDef(aggregate = "sum"),
        percent_completion = colDef(aggregate = js_func,
                                    format = colFormat(
                                      percent = TRUE,
                                      digits = 1))
      )
    )
  }) %>% bindCache(values$orig_cha_data,
                   values$filter_cha_chv_data,
                   cache = cachem::cache_disk("./myapp-cache"))

  output$cha_table = renderReactable({
    if(input$submit == 0){
      data <- values$orig_cha_data
    }else{
      data <- values$filter_cha_data
    }
    reactable(data %>%
                dplyr::select(
                  wid,
                  subcounty,
                  ward,
                  community_health_unit,
                  internet_conn_cha,
                  number_chv_supervise
                ) %>%
                distinct(wid,.keep_all = TRUE))
  })


  output$chv_table = renderReactable({
    if(input$submit == 0){
      data <- values$orig_chv_data
    }else{
      data <- values$filter_chv_data
    }
    reactable(data)
  }) %>% bindCache(values$orig_chv_data,
                   values$filter_chv_data,
                   cache = cachem::cache_disk("./myapp-cache"))


  output$cha_map_plot <- renderLeaflet({
    if(input$submit == 0){
      data <- values$orig_map_data
    }else{
      data <- values$filter_map_data
    }

    content_placeholder <- paste0("Household ID: {hh_id}<br/>",
                                  "Ward: {ward}<br/>",
                                  "Village: {village}<br/>",
                                  "CHA ID: {wid_cha}<br/>",
                                  "CHV ID: {wid}")

    data <- data  %>%
      dplyr::mutate(content = glue::glue(content_placeholder))

    pal <- colorFactor(palette = "Dark2",
                        domain = unique(data$wid_cha))

    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        lng=~Longitude,
        stroke = FALSE,
        fillOpacity = 0.5,
        radius = 3,
        lat=~Latitude,
        popup=~content,
        color = ~pal(wid_cha))
  }) %>% bindCache(values$orig_map_data,
                   values$filter_map_data,
                   cache = cachem::cache_disk("./myapp-cache"))

}

## To be copied in the UI
# mod_fieldworker_performance_ui("fieldworker_performance_1")

## To be copied in the server
# mod_fieldworker_performance_server("fieldworker_performance_1")
