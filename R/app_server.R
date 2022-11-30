#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import paws
#' @import dplyr
#' @import data.table
#' @import glue
#' @import magrittr
#' @import purrr
#' @import lubridate
#' @import data.table
#' @noRd
app_server <- function(input, output, session) {
  callModule(mod_progress_server, 'household')
  callModule(mod_anomalies_server, 'anomalies')
  callModule(mod_internet_coverage_server, 'household')
  callModule(mod_fieldworker_performance_server, 'registration')
  waiter_hide()
  # callModule(mod_submission_by_day_server, 'submission_by_day_plot')
  # Your application server logic
}
