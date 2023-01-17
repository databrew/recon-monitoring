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
      div("Author: atediarjo@gmail.com"),
      div("Reviewed by : joe@brew.cc, xing@brew.cc"),
      br(),
      h2("About"),
      div("This dashboard will monitor Kwale Recon. It tracks recon Progress, Internet Connectivity, Fieldworker Performance, Anomalies of Kwale recon"),
      div("\n"),
      div("- Progress: Tracks progress of how many household forms submitted, submission location, by how many CHVs, and % comparison to our recon target"),
      div("- Fieldworker Performance: Tracks fieldworker performances (% to recon target) based on hierarchial structure from CHA -> CHV -> Household Forms Collection"),
      div("- Anomalies: Helps our anomalies resolver group to track their anomalies bug bashing"),
      div("- Internet Coverage: Tracks internet connectivity of data collection process"),
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

