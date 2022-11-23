#' @import shiny
#' @import shinydashboard
#' @import shinyURL
#' @import shinyWidgets
#' @import shinyjs
#' @import ggplot2
#' @import tidyr
#' @import ggradar
#' @import htmltools
#' @import RColorBrewer
#' @import sp
#' @import leaflet
#' @import plotly
#' @import mapview
#' @import forcats
#' @import stringr
#' @import waiter
#' @import shinyalert
#' @import scales
#' @importFrom shiny NS tagList
app_ui <- function() {

  # HEADER
  header <- dashboardHeader(title  = 'Recon Monitoring')
  # SIDEBAR
  sidebar <- dashboardSidebar(
    width = 230,
    sidebarMenu(
      id = 'sidebar',
      width = 230,
      menuItem(
        text = 'About',
        tabName = 'about'
      ),
      menuItem(
        id = "vizualizationIDbar",
        text = 'Visualizations',
        tabName = 'recon_vis',
        startExpanded = TRUE,
        menuSubItem(
          text="Progress",
          tabName="progress",
          icon = shiny::icon("angle-right")
        ),
        menuSubItem(
          text="Anomalities",
          tabName="anomalities",
          icon = shiny::icon("angle-right")
        ),
        menuSubItem(
          text="Internet Coverage",
          tabName="internet_coverage",
          icon = shiny::icon("angle-right")
        ),
        menuSubItem(
          text="Fieldworker Performance",
          tabName="fieldworker_performance",
          icon = shiny::icon("angle-right")
        )
      ),
      menuItem(
        text = 'Documentation',
        tabName = 'docu'
      ),
      menuItem(
        text = 'Download full data',
        href = 'https://datacatalog.worldbank.org/dataset/hefpi'
      )
    )
  )
  body <- dashboardBody(
    use_waiter(),
    waiter_show_on_load(color = "#002244"),
    golem_add_external_resources(),
    tabItems(
      tabItem(tabName = "about",
              h1("Welcome to the Recon Monitoring RShiny App!"),
              br(),
              h2("Guidelines:"),
              br(),
              h2("References:")
      ),
      tabItem(
        tabName="progress",
        fluidPage(
          fluidRow(
            h2("Kwale Monitoring Dashboard"),
            br()
          ),
          fluidRow(
            mod_progress_ui("progress")
          )
        )
      )
    )
  )

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(header = header,
                  sidebar=sidebar,
                  body=body)
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    golem::favicon(),
    golem::activate_js(),
    # tags$link(rel="stylesheet", type="text/css", href="www/custom.css"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "reconmonitoring"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
