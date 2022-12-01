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
          style="z-index:1002;")
      ),

      fluidRow(
        column(3, actionBttn(
          ns("submit"),
          "Submit Selection",
          color = "primary",
          style = 'minimal'))
      ),
      br(),
      fluidRow(
        column(6, box(
          title = 'CHA Table',
          DT::dataTableOutput(ns('cha_table'), height = 400),
          width = NULL, solidHeader= TRUE, )),
        column(6, box(
          title = 'CHV Table',
          DT::dataTableOutput(ns('chv_table'), height = 400),
          width = NULL, solidHeader= TRUE, ))
        )
      ),
      fluidRow(
        column(6, box(
          title = 'Household Registration Form Submissions from CHA-supervised CHVs',
          leafletOutput(ns('cha_map_plot'), height = 400),
          width = NULL, solidHeader= TRUE)),
        column(6, box(
          title = 'Household Registration Form Submissions from CHV',
          leafletOutput(ns('chv_map_plot'), height = 400),
          width = NULL, solidHeader= TRUE))
    )
  )
}

#' fieldworker_performance Server Functions
#'
#' @noRd
mod_fieldworker_performance_server <- function(input, output, session){
  ns <- session$ns

  registration <-  get_registartion_forms()
  hh <- get_household_forms() %>%
    dplyr::select(-end_time)

  cha_data <- registration %>%
    dplyr::filter(worker_type == 'CHA') %>%
    dplyr::select(wid,
                  w_first_name,
                  w_last_name,
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
                  w_first_name,
                  w_last_name,
                  community_health_unit = community_health_unit_chv,
                  username,
                  villages = villages_chv,
                  subcounty = sub_county_chv,
                  ward = ward_chv,
                  num_households)


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
                         num_household_form_submitted = n()),
                     by = "wid") %>%
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
    dplyr::left_join(chv_data_summarised %>%
                       dplyr::select(wid_chv = wid,
                                     wid_cha,
                                     num_household_form_submitted),
                     by = c("wid" = "wid_cha")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      wid_chv = ifelse(is.na(wid_chv), as.character(NA),
                            paste0(wid_chv, collapse = ";")),
      total_household_form_submitted = sum(
        num_household_form_submitted, na.rm = T))

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
    dplyr::left_join(chv_data %>%
                       dplyr::select(wid, wid_cha) %>%
                       distinct(), by = "wid")



  cha_list <- cha_data %>%
    .$wid %>%
    unique()

  chv_list <- chv_data %>%
    .$wid %>%
    unique()


  values <- reactiveValues(
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

  observeEvent(input$submit, {
    # values$filter_cha_data <- values$orig_cha_data %>%
    #   dplyr::filter(wid %in% input$cha_wid)
    values$filter_cha_data <- values$orig_cha_data %>%
      dplyr::filter(wid %in% input$cha_wid)

    values$filter_chv_data <- values$orig_chv_data %>%
      dplyr::filter(wid %in% input$chv_wid)

    values$filter_map_data <- values$orig_map_data %>%
      dplyr::filter(wid_cha %in% input$cha_wid)

  }, ignoreNULL=FALSE)


  output$cha_table = DT::renderDataTable({
    if(input$submit == 0){
      data <- values$orig_cha_data
    }else{
      data <- values$filter_cha_data
    }

    DT::datatable(data,
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtip',
                    searching = FALSE,
                    pageLength = 5,
                    scrollX=TRUE,
                    buttons = list(
                      list(extend = "csv", text = "Download Full Results", filename = "data",
                           exportOptions = list(
                             modifier = list(page = "all")
                           )
                      )
                    )
                  )
    )
  })


  output$chv_table = DT::renderDataTable({
    if(input$submit == 0){
      data <- values$orig_chv_data
    }else{
      data <- values$filter_chv_data
    }


    DT::datatable(data,
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtip',
                    searching = FALSE,
                    pageLength = 5,
                    scrollX=TRUE,
                    buttons = list(
                      list(extend = "csv", text = "Download Full Results", filename = "data",
                           exportOptions = list(
                             modifier = list(page = "all")
                           )
                      )
                    )
                  )
    )
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
                                  "Worker ID: {wid}")

    data <- data  %>%
      as_tibble(.name_repair = "universal") %>%
      dplyr::filter(!is.na(Latitude), !is.na(Longitude)) %>%
      dplyr::mutate(content = glue::glue(content_placeholder))


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


  output$chv_map_plot <- renderLeaflet({
    if(input$submit == 0){
      data <- values$orig_map_data
    }else{
      data <- values$filter_map_data
    }

    content_placeholder <- paste0("Household ID: {hh_id}<br/>",
                                  "Ward: {ward}<br/>",
                                  "Village: {village}<br/>",
                                  "Worker ID: {wid}")
    data <- data  %>%
      as_tibble(.name_repair = "universal") %>%
      dplyr::filter(!is.na(Latitude), !is.na(Longitude)) %>%
      dplyr::mutate(content = glue::glue(content_placeholder))




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

}

## To be copied in the UI
# mod_fieldworker_performance_ui("fieldworker_performance_1")

## To be copied in the server
# mod_fieldworker_performance_server("fieldworker_performance_1")
