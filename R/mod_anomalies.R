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
        column(6, box(
          title = 'Anomalies by Type',
          plotlyOutput(ns('anomalies_by_type_plot'), height = 400),
          width = NULL, solidHeader= TRUE,))
      ),
      fluidRow(
        column(12, box(
          title = 'Anomalies Table',
          DT::dataTableOutput(ns("anomalies_table"), height = 400),
          width = NULL, solidHeader= TRUE,))
      )
    )

  )
}

#' anomalies Server Functions
#'
#' @noRd
mod_anomalies_server <- function(input, output, session){
  data <-  get_anomalies_data()

  values <- reactiveValues(
    orig_data = data,
  )

  output$anomalies_by_type_plot <- renderPlotly({
    summary <- values$orig_data %>%
      dplyr::group_by(type) %>%
      dplyr::summarise(n = n())
    ggplotly(
        summary %>%
          ggplot(aes(y = type, x = n, fill = type)) +
          geom_col(alpha = 0.9) +
          theme_minimal() +
          labs(y = "", x = "") +
          theme(legend.position = "none") +
          scale_fill_brewer(palette="Dark2")
    )
  })

  output$anomalies_table = DT::renderDataTable({
    DT::datatable(values$orig_data,
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

}

## To be copied in the UI
# mod_anomalies_ui("anomalies_1")

## To be copied in the server
# mod_anomalies_server("anomalies_1")
