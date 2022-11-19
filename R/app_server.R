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
#' @noRd
app_server <- function(input, output, session) {
  Sys.setenv(AWS_PROFILE = "dbrew-prod")
  callModule(mod_progress_server, 'progress')
  # callModule(mod_submission_by_day_server, 'submission_by_day_plot')
  # Your application server logic
}
