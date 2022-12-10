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
  # shinyOptions(cache = cachem::cache_disk("./myapp-cache"))
  callModule(mod_fieldworker_performance_server, 'registration')
  callModule(mod_landing_page_server, 'landing')
  callModule(mod_progress_server, 'household')
  callModule(mod_anomalies_server, 'anomalies')
  callModule(mod_internet_coverage_server, 'household')
  waiter_hide()

}
