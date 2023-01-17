#' anomalies UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_anomalies_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(3, pickerInput(ns("anomalies_type"),
                              "Select Anomalies Type:", "",
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
      fluidRow(
        column(6, box(
          title = 'Number of Anomalies by Type',
          plotlyOutput(ns('anomalies_by_type_plot'), height = 400) %>% shinycssloaders::withSpinner(),
          width = NULL, solidHeader= TRUE,)),
        column(6, box(
          title = 'Number of Anomalies by Date',
          plotlyOutput(ns('anomalies_by_date_plot'), height = 400) %>% shinycssloaders::withSpinner(),
          width = NULL, solidHeader= TRUE,))
      ),
      fluidRow(
        column(12, box(
          title = 'Anomalies Identifed',
          tags$button(
            tagList(icon("download"), "Download"),
            onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')",
                              ns('anomalies_table'),
                              "anomalies.csv")
          ),
          reactableOutput(ns('anomalies_table'), height = 400) %>% shinycssloaders::withSpinner(),
          width = NULL, solidHeader= TRUE, ))
      )
    )

  )
}

#' anomalies Server Functions
#'
#' @noRd
mod_anomalies_server <- function(input, output, session){
  # get data
  anomalies_data = get_anomalies_data()
  anomalies_summary = get_anomalies_summary_by_date()

  # list of default reactive values for picker
  values <- reactiveValues(
    anomalies_list = anomalies_data$type %>% unique()
  )

  # orig values
  orig_anomalies_data <- reactive({anomalies_data})
  orig_anomalies_summary <- reactive({anomalies_summary})


  # update picker input based on default
  observe({
    updatePickerInput(session, "anomalies_type",
                      choices = values$anomalies_list,
                      selected = values$anomalies_list)
  })

  ## submit events trigger
  # filter anomalies detail
  filter_anomalies_data <- eventReactive(input$submit, {
    orig_anomalies_data() %>%
      dplyr::filter(type %in% input$anomalies_type)
  }, ignoreNULL=FALSE)

  # filter anomalies summary
  filter_anomalies_summary <- eventReactive(input$submit, {
    orig_anomalies_summary() %>%
      dplyr::filter(type %in% input$anomalies_type)
  }, ignoreNULL=FALSE)

  # get anomalies by date plot
  output$anomalies_by_date_plot <- renderPlotly({
    if(input$submit == 0) {
      data <- orig_anomalies_summary()
    }else{
      data <- filter_anomalies_summary()
    }

    ggplotly(data %>%
               ggplot2::ggplot(aes(x = run_date, y = number_of_anomalies, color = type)) +
               geom_line() +
               geom_point() +
               theme_minimal() +
               theme(
                 panel.grid.major.x = element_blank(),
                 legend.position = "none") +
               labs(x = "", y = "") +
               scale_fill_brewer(palette="Dark2")
             )
  })

  # get anomalies by type plot
  output$anomalies_by_type_plot <- renderPlotly({
    if(input$submit == 0) {
      data <- orig_anomalies_data()
    }else{
      data <-  filter_anomalies_data()
    }
    summary <- data %>%
      dplyr::group_by(type) %>%
      dplyr::summarise(n = n())
    ggplotly(
        summary %>%
          ggplot(aes(y = type, x = n, fill = type)) +
          geom_col(alpha = 0.9, width = 0.5) +
          theme_minimal() +
          labs(y = "", x = "") +
          theme(legend.position = "none") +
          scale_fill_brewer(palette="Dark2")
    )
  })

  # get anomalies table
  output$anomalies_table = renderReactable({
    if(input$submit == 0) {
      data <- orig_anomalies_data()
    }else{
      data <-  filter_anomalies_data()
    }

    reactable(data = data)
  })

}

## To be copied in the UI
# mod_anomalies_ui("anomalies_1")

## To be copied in the server
# mod_anomalies_server("anomalies_1")
