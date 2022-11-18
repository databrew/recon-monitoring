#' submission_by_day UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_submission_by_day_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotlyOutput(
      ns('submission_by_day_plot'),
      height = 400)
  )
}

#' submission_by_day Server Functions
#'
#' @noRd
mod_submission_by_day_server <- function(input, output, session){
    ns <- session$ns

    svc <- paws::s3()
    filepath <- glue::glue(tempdir,"reconaregistration.csv")
    svc$download_file(
      Bucket = "dbrew-testdatabrew.org",
      Key = "kwale/raw-form/reconaregistration/reconaregistration.csv",
      Filename = filepath)

    data <-  read.csv(filepath) %>%
      tibble::as_tibble(.name_repair = "unique")
    values <- reactiveValues(
      reconaregistration = data)


    output$submission_by_day_plot <- renderPlotly({
      p <- ggplotly(values$reconaregistration %>%
        dplyr::mutate(day = lubridate::as_date(SubmissionDate)) %>%
        dplyr::group_by(day) %>%
        dplyr::summarise(n = n()) %>%
        ggplot(aes(x = day, y = n)) + geom_bar(stat = "identity") +
        theme_minimal() +
        labs(x = "day", y = "number of submissions"))
      p
    })
}

## To be copied in the UI
# mod_submission_by_day_ui("submission_by_day_1")

## To be copied in the server
# mod_submission_by_day_server("submission_by_day_1")
