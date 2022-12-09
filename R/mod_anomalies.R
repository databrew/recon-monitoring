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
          plotlyOutput(ns('anomalies_by_type_plot'), height = 400),
          width = NULL, solidHeader= TRUE,)),
        column(6, box(
          title = 'Number of Anomalies by Date',
          plotlyOutput(ns('anomalies_by_date_plot'), height = 400),
          width = NULL, solidHeader= TRUE,))
      ),
      fluidRow(
        column(12, box(
          title = 'CHV Raw Table',
          tags$button(
            tagList(icon("download"), "Download"),
            onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')",
                              ns('anomalies_table'),
                              "anomalies.csv")
          ),
          reactableOutput(ns('anomalies_table'), height = 400),
          width = NULL, solidHeader= TRUE, ))
      )
    )

  )
}

#' anomalies Server Functions
#'
#' @noRd
mod_anomalies_server <- function(input, output, session){
  anomalies_data = get_anomalies_data()
  anomalies_summary = get_anomalies_summary_by_date()

  # orig values
  values <- reactiveValues(
    anomalies_data =  anomalies_data,
    anomalies_summary = anomalies_summary,
    anomalies_list = anomalies_data$type %>% unique()
  )

  # filtered values
  filtered_values <- reactiveValues(
    anomalies_data =  anomalies_data,
    anomalies_summary = anomalies_summary,
    anomalies_list = anomalies_data$type %>% unique()
  )



  observe({
    updatePickerInput(session, "anomalies_type",
                      choices = values$anomalies_list,
                      selected = values$anomalies_list)
  })

  observeEvent(input$submit, {
    # values$filter_cha_data <- values$orig_cha_data %>%
    #   dplyr::filter(wid %in% input$cha_wid)
    filtered_values$anomalies_data <- values$anomalies_data %>%
      dplyr::filter(type %in% input$anomalies_type)

    filtered_values$anomalies_summary <- values$anomalies_summary %>%
      dplyr::filter(type %in% input$anomalies_type)
  }, ignoreNULL=FALSE)


  output$anomalies_by_date_plot <- renderPlotly({
    if(input$submit == 0) {
      data <- values$anomalies_summary
    }else{
      data <- filtered_values$anomalies_summary
    }

    ggplotly(data %>%
               group_by(run_date) %>%
               summarise(number_of_anomalies = sum(number_of_anomalies)) %>%
               ggplot2::ggplot(aes(x = run_date, y = number_of_anomalies)) +
               geom_line(color = 'darkblue') +
               geom_point() +
               theme_minimal() +
               theme(
                 panel.grid.major.x = element_blank(),
                 legend.position = "none") +
               labs(x = "", y = "") +
               ylim(0, 400)
             )
  })

  output$anomalies_by_type_plot <- renderPlotly({
    if(input$submit == 0) {
      data <- values$anomalies_data
    }else{
      data <-  filtered_values$anomalies_data
    }
    summary <- data %>%
      dplyr::group_by(type) %>%
      dplyr::summarise(n = n())
    ggplotly(
        summary %>%
          ggplot(aes(x = type, y = n, fill = type)) +
          geom_col(alpha = 0.9, width = 0.5) +
          theme_minimal() +
          labs(y = "", x = "") +
          theme(legend.position = "none") +
          scale_fill_brewer(palette="Dark2")
    )
  })

  output$anomalies_table = renderReactable({
    if(input$submit == 0) {
      data <- values$anomalies_data
    }else{
      data <-  filtered_values$anomalies_data
    }

    reactable(data = data)
  })

}

## To be copied in the UI
# mod_anomalies_ui("anomalies_1")

## To be copied in the server
# mod_anomalies_server("anomalies_1")
