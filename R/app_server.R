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
  callModule(mod_submission_map_server, 'submission_map')
  # callModule(mod_submission_by_day_server, 'submission_by_day_plot')
  # Your application server logic
}
