# Module radar
#' @title mod_recent_radar.R
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @keywords internal
#' @export
# UI FOR MOST RECENT VALUE RADAR PLOT
mod_recent_radar_ui <- function(id){
  # let leaflet know that selections should persist
  # options(persistent = TRUE)
  ns <- NS(id)
  # tagList(
  fluidRow(
    column(6,
           leafletOutput(
             ns('recent_sub_mean_plot'),
             height = 400)
    )
  )
}

# SERVER FOR MOST RECENT VALUE MAP
mod_recent_radar_server <- function(input, output, session){

  svc <- paws::s3()
  filepath <- glue::glue(tempdir,"reconaregistration.csv")
  svc$download_file(
    Bucket = "dbrew-testdatabrew.org",
    Key = "kwale/raw-form/reconaregistration/reconaregistration.csv",
    Filename = filepath)

  data <-  read.csv(filepath) %>%
    tibble::as_tibble(.name_repair = "unique") %>%
    dplyr::filter(!is.na(Longitude))
  values <- reactiveValues(
    reconaregistration = data
  )




  # ---- RENDER PLOT FROM REACTIVE DATA ---- #
  output$recent_sub_mean_plot <- renderLeaflet({
    mapview(
      values$reconaregistration,
      xcol = "Longitude",
      ycol="Latitude",
      crs = 4269, grid = FALSE)@map
  })
}
