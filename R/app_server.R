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
#' @import shinymanager
#' @import zoo
#' @noRd
app_server <- function(input, output, session) {
  # caching (future implementation)
  # shinyOptions(cache = cachem::cache_disk("./myapp-cache"))

  # aws credentials
  sm <- paws::secretsmanager()
  creds <- sm$get_secret_value('prod/recon-admin') %>%
    .$SecretString %>%
    jsonlite::parse_json(.)
  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(
      data.frame(
        user = c(creds$username), # mandatory
        password = c(creds$password), # mandatory
        admin = c(TRUE),
        comment = "Recon admin authentication",
        stringsAsFactors = FALSE
      )
    )
  )
  # call modules for triggering UIs and Servers
  callModule(mod_fieldworker_server, 'registration')
  callModule(mod_landing_page_server, 'landing')
  callModule(mod_progress_server, 'household')
  callModule(mod_anomalies_server, 'anomalies')
  callModule(mod_internet_coverage_server, 'household')
  waiter_hide()
}
