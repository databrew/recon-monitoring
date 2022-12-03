#' fieldworker_performance UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_landing_page_ui <- function(id){
  ns <- NS(id)
  tagList(
      h1("Recon Monitoring Dashboard"),
      div("Property of DataBrew LLC."),
      br(),
      div("@Author: atediarjo@gmail.com"),
      div("@Reviewed by : joe@brew.cc, xing@brew.cc"),
      br(),
      h2("About"),
      div("This dashboard will monitor Kenya Recon process. It will track Progress, Internet Connectivity, FieldWorker Performance, Anomalies of our CHAs and CHVs collecting houseold data"),
      br(),
      h2("Data Refresh Cycle"),
      div("- Hourly from 8AM-5PM East African Time"),
      div("- Once at 12AM East African Time"),
      br(),
      h3("Data Refresh Update"),
      reactableOutput(ns("update_table")),
      br(),
      h2("Resources"),
      div("GitHub Repository: https://github.com/databrew/recon-monitoring")
    )

}

#' fieldworker_performance Server Functions
#'
#' @noRd
mod_landing_page_server <- function(input, output, session){
  ns <- session$ns
  values <- reactiveValues(
    update_table = get_object_update_time()
  )

  output$update_table = renderReactable({
    reactable(data = values$update_table)
  })




}

