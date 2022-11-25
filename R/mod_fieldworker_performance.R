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
        column(3, pickerInput(ns("cha_wid"), "Select CHA by ID:", "",
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE,
                                             `live-search` = TRUE)),
               style="z-index:1002;"),
        column(3, pickerInput(ns("chv_wid"), "Select CHV by ID:", "",
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE,
                                             `live-search` = TRUE)),
               style="z-index:1002;"),
        column(1, actionBttn(ns("submit"),
                             "Submit Selection",
                             color = "primary",
                             style = 'simple'))
      ),

      fluidRow(
        column(6, box(
          title = 'Data about CHA',
          DT::dataTableOutput(ns('data_cha'), height = 400),
          width = NULL, status = "primary", solidHeader = TRUE)),
        column(6, box(
          title = 'Data about CHV',
          DT::dataTableOutput(ns('data_chv'), height = 400),
          width = NULL, status = "primary", solidHeader = TRUE))
      ),
      fluidRow(
        column(6, box(
          title = 'Household Registration Form Submissions from CHA-supervised CHVs',
          leafletOutput(ns('cha_map_plot'), height = 400),
          width = NULL, status = "primary", solidHeader = TRUE)),
        column(6, box(
          title = 'Household Registration Form Submissions from CHV',
          leafletOutput(ns('chv_map_plot'), height = 400),
          width = NULL, status = "primary", solidHeader = TRUE))
      )
    )
  )
}

#' fieldworker_performance Server Functions
#'
#' @noRd
mod_fieldworker_performance_server <- function(input, output, session){
  ns <- session$ns

  filename <- tempfile(fileext = '.csv')
  data <-  get_s3_data(
    s3obj = paws::s3(),
    bucket = 'databrew.org',
    object_key = "kwale/clean-form/reconaregistrationtraining/reconaregistrationtraining.csv",
    filename = filename) %>%
    read.csv(.) %>%
    tibble::as_tibble(.name_repair = "unique") %>%
    dplyr::mutate(wid = as.character(wid))


  cha_data <- data %>%
    dplyr::filter(worker_type == 'CHA') %>%
    dplyr::select(
      wid,
      w_first_name,
      w_last_name,
      sub_county_cha,
      ward_cha,
      community_health_unit_cha,
      number_chv_supervise,
      num_households,
      internet_conn_cha,
      Latitude,
      Longitude)

  chv_data <- data %>%
    dplyr::filter(worker_type == 'CHV') %>%
    mutate(wid_cha = ifelse(is.na(cha_wid_qr), cha_wid_manual, cha_wid_qr)) %>%
    dplyr::select(
      wid,
      wid_cha,
      w_first_name,
      w_last_name,
      sub_county_chv,
      ward_chv,
      community_health_unit_chv,
      villages_chv,
      num_households,
      internet_conn_chv,
      Latitude,
      Longitude)

  cha_list <- cha_data %>%
    .$wid %>%
    unique()

  chv_list <- chv_data %>%
    .$wid %>%
    unique()

  values <- reactiveValues(
    orig_cha_data = cha_data,
    orig_chv_data = chv_data,
    cha_list = cha_list,
    chv_list = chv_list,
    filter_cha_data = cha_data,
    filter_chv_data = chv_data,
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
    values$filter_cha_data <- values$orig_cha_data %>%
      dplyr::filter(wid %in% input$cha_wid)

    values$filter_chv_data <- values$orig_chv_data %>%
      dplyr::filter(wid %in% input$chv_wid)
  }, ignoreNULL=FALSE)


  output$data_cha = DT::renderDataTable({
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
                    ),
                    fixedColumns = list(leftColumns = 2)
                  )
    )
  })

  output$data_chv = DT::renderDataTable({
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
                    ),
                    fixedColumns = list(leftColumns = 2)
                  )
                )
  })


  output$cha_map_plot <- renderLeaflet({
    if(input$submit == 0){
      data <- values$orig_chv_data
    }else{
      data <- values$orig_chv_data %>%
        dplyr::filter(wid_cha %in% input$cha_wid)
    }

    data <- data %>%
      as_tibble(.name_repair = "universal") %>%
      dplyr::filter(!is.na(Latitude), !is.na(Longitude))

    if(nrow(data) > 0){
      mapv <- mapview(
        data,
        xcol = "Longitude",
        ycol="Latitude",
        crs = 4269,
        grid = FALSE)

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

  output$chv_map_plot <- renderLeaflet({
    if(input$submit == 0){
      data <- values$orig_chv_data
    }else{
      data <- values$orig_chv_data %>%
        dplyr::filter(wid %in% input$chv_wid)
    }

    data <- data %>%
      as_tibble(.name_repair = "universal") %>%
      dplyr::filter(!is.na(Latitude), !is.na(Longitude))

    if(nrow(data) > 0){
      mapv <- mapview(
        data,
        xcol = "Longitude",
        ycol="Latitude",
        crs = 4269,
        grid = FALSE)

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

}

## To be copied in the UI
# mod_fieldworker_performance_ui("fieldworker_performance_1")

## To be copied in the server
# mod_fieldworker_performance_server("fieldworker_performance_1")
