#' fieldworker_performance UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fieldworker_ui <- function(id){
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
          footer = 'Household Form submission grouped by each CHA. Each CHA contains CHVs that submits household forms, click to see more details',
          leafletOutput(ns('cha_map_plot'), height = 400) %>%
            shinycssloaders::withSpinner(),
          width = NULL, solidHeader= TRUE)),
        column(6, box(
          title = 'Fieldworker Summary Table',
          footer = 'This table is joined based on CHVs that works under CHA, CHV without CHA assigned will not be included in this table',
          tags$button(
            tagList(icon("download"), "Download"),
            onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')",
                              ns('cha_chv_summary_table'),
                              "summary_performance_cha.csv")
          ),
          reactableOutput(ns('cha_chv_summary_table'), height = 375) %>%
            shinycssloaders::withSpinner(),
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
          reactableOutput(ns('cha_table'), height = 400) %>%
            shinycssloaders::withSpinner(),
          width = NULL, solidHeader= TRUE, )),
        column(6, box(
          title = 'CHV Raw Table',
          tags$button(
            tagList(icon("download"), "Download"),
            onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')",
                              ns('chv_table'),
                              "chv.csv")
          ),
          reactableOutput(ns('chv_table'), height = 400) %>%
            shinycssloaders::withSpinner(),
          width = NULL, solidHeader= TRUE, ))
      )
    )
  )
}


mod_fieldworker_server <- function(input, output, session){
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

  # list of default reactive values for picker
  values <- reactiveValues(
    cha_list = cha_list,
    chv_list = chv_list
  )

  # get original data in reactives
  orig_cha_data <- reactive({cha_data_summarised})
  orig_chv_data <- reactive({chv_data_summarised})
  orig_map_data <- reactive({map_data})

  # update picker input to align with choices available
  observe({
    updatePickerInput(session, "cha_wid",
                      choices = sort(values$cha_list),
                      selected = values$cha_list)
    updatePickerInput(session, "chv_wid",
                      choices = sort(values$chv_list),
                      selected = values$chv_list)
  })

  # a dependent filter to trigger dropdown of chv to align with cha
  observeEvent(input$cha_wid, {
    f <- orig_chv_data() %>%
      dplyr::filter(wid_cha %in% input$cha_wid) %>%
      .$wid %>%
      unique()
    updatePickerInput(session, "chv_wid",
                      choices = sort(f),
                      selected = f)
  }, ignoreNULL = FALSE)


  ## submit events trigger
  # filter CHAs
  filter_cha_data <- eventReactive(input$submit, {
    orig_cha_data() %>%
      dplyr::filter(wid %in% input$cha_wid)
  }, ignoreNULL=FALSE)
  # filtered CHVs
  filter_chv_data <- eventReactive(input$submit, {
    orig_chv_data() %>%
      dplyr::filter(wid %in% input$chv_wid)
  }, ignoreNULL=FALSE)
  # filtered CHV under CHA
  filter_cha_chv_data <- eventReactive(input$submit, {
    orig_cha_data() %>%
      dplyr::filter(wid %in% input$cha_wid) %>%
      dplyr::filter(wid_chv %in% input$chv_wid)
  }, ignoreNULL=FALSE)
  # filter map data
  filter_map_data <- eventReactive(input$submit, {
    orig_map_data() %>%
      dplyr::filter(wid_cha %in% input$cha_wid) %>%
      dplyr::filter(wid %in% input$chv_wid)
  }, ignoreNULL=FALSE)

  # output summary table of CHV under CHAs
  output$cha_chv_summary_table = renderReactable({
    if(input$submit == 0){
      data <- orig_cha_data()
    }else{
      data <- filter_cha_chv_data()
    }
  # row-calculation function
    js_func <- JS("function(values, rows) {
        let totalSubmitted = 0
        let totalTarget = 0
        rows.forEach(function(row) {
          totalTarget += row['Number Households']
          totalSubmitted += row['Number Households Forms Submitted']
        })
        return totalSubmitted / totalTarget
      }")
    reactable(
      data = data %>%
        dplyr::select(
          `CHA Worker ID` = wid,
          `CHV Worker ID` = wid_chv,
          `Number Households Forms Submitted` = num_household_form_submitted,
          `Number Households` = num_households) %>%
        dplyr::mutate(`Percent Completion` =
                        `Number Households Forms Submitted`/`Number Households`),
      groupBy = c("CHA Worker ID"),
      columns = list(
        `Number Households Forms Submitted` = colDef(aggregate = "sum"),
        `Number Households` = colDef(aggregate = "sum"),
        `Percent Completion` = colDef(aggregate = js_func,
                                    format = colFormat(
                                      percent = TRUE,
                                      digits = 1))
      ),
      defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
    )
  })

  # function to render CHA table
  output$cha_table = renderReactable({
    if(input$submit == 0){
      data <- orig_cha_data()
    }else{
      data <- filter_cha_data()
    }
    reactable(data %>%
                dplyr::select(
                  `CHA Worker ID` = wid,
                  subcounty,
                  ward,
                  `CHU` = community_health_unit,
                  `Internet Connection` = internet_conn_cha,
                  `Number CHV Supervise` = number_chv_supervise
                ) %>%
                distinct(`CHA Worker ID`,.keep_all = TRUE))
  })

  # function to render CHV table
  output$chv_table = renderReactable({
    if(input$submit == 0){
      data <- orig_chv_data()
    }else{
      data <- filter_chv_data()
    }
    reactable(data %>%
                dplyr::select(`CHV Worker ID` = wid,
                              `CHA Worker ID` = wid_cha,
                              `CHU` = community_health_unit,
                              username,
                              villages,
                              subcounty,
                              ward,
                              `Number Households Target` = num_households,
                              `Internet Connection` = internet_conn_chv))
  })

  # function to render map plots
  output$cha_map_plot <- renderLeaflet({
    if(input$submit == 0){
      data <- orig_map_data()
    }else{
      data <- filter_map_data()
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
