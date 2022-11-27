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
          style = 'simple'))
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

  filename <- tempfile(fileext = '.csv')
  registration <-  get_s3_data(
    s3obj = paws::s3(),
    bucket = 'databrew.org',
    object_key = "kwale/clean-form/reconaregistrationtraining/reconaregistrationtraining.csv",
    filename = filename) %>%
    read.csv(., row.names = 1) %>%
    tibble::as_tibble(.name_repair = "unique") %>%
    dplyr::mutate(wid = as.character(wid),
                  wid_cha = as.character(
                    ifelse(is.na(cha_wid_qr),
                    cha_wid_manual,
                    cha_wid_qr)),
                  Latitude = as.numeric(Latitude),
                  Longitude = as.numeric(Longitude))

  cha_data <- registration %>%
    dplyr::filter(worker_type == 'CHA') %>%
    dplyr::select(wid,
                  w_first_name,
                  w_last_name,
                  community_health_unit = community_health_unit_chv,
                  username,
                  Longitude,
                  Latitude,
                  villages = villages_cha,
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
                  Longitude,
                  Latitude,
                  villages = villages_chv,
                  subcounty = sub_county_chv,
                  ward = ward_chv,
                  num_households,
                  internet_conn_chv)

  hh <- get_s3_data(
    s3obj = paws::s3(),
    bucket = 'databrew.org',
    object_key = "kwale/clean-form/reconbhouseholdtraining/reconbhouseholdtraining.csv",
    filename = filename) %>%
    read.csv(., row.names = 1) %>%
    tibble::as_tibble(.name_repair = "unique") %>%
    tidyr::drop_na(wid_qr) %>%
    dplyr::mutate(Latitude = as.numeric(Latitude),
                  Longitude = as.numeric(Longitude))


  cha_list <- cha_data %>%
    .$wid %>%
    unique()

  chv_list <- chv_data %>%
    .$wid %>%
    unique()

  values <- reactiveValues(
    orig_cha_data = cha_data,
    filter_cha_data = cha_data,
    orig_chv_data = chv_data,
    filter_chv_data = chv_data,
    orig_hh_data = hh,
    filter_hh_data = hh,
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
      dplyr::filter(wid %in% input$wid)

    values$filter_chv_data <- values$orig_chv_data %>%
      dplyr::filter(wid %in% input$wid)

  }, ignoreNULL=FALSE)


  output$cha_table = DT::renderDataTable({
    if(input$submit == 0){
      data <- values$orig_cha_data
    }else{
      data <- values$filter_cha_data
    }

    data <- data %>%
      dplyr::select(-Latitude, -Longitude)

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

    data <- data %>%
      dplyr::select(-Latitude, -Longitude)

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

    data <- values$orig_hh_data %>%
        dplyr::filter(wid_qr %in% values$cha_list)


    content_placeholder <- paste0("Household ID: {hh_id}<br/>",
                                  "Ward: {ward}<br/>",
                                  "Village: {village}<br/>",
                                  "Worker ID: {wid_qr}")

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

    data <- values$orig_hh_data %>%
      dplyr::filter(wid_qr %in% values$chv_list)

    content_placeholder <- paste0("Household ID: {hh_id}<br/>",
                                  "Ward: {ward}<br/>",
                                  "Village: {village}<br/>",
                                  "Worker ID: {wid_qr}")
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
