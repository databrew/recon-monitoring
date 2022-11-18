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
    golem_add_external_resources(),
    tabItems(
      tabItem(tabName = "about",
              h1("Welcome to the Recon Monitoring RShiny App!"),
              div("This app will help you go through Synapse Table-Attached Files and"),
              div("based on your desired visualization for the files, the app will help you annotate each"),
              div("scores/labels/information about the visualizations"),
              br(),
              h2("How-To:"),
              div("1. Go to the 'Annotator-App' Tab"),
              div("2. You can go through each image by using arrows"),
              div("3. Score accordingly based on each prompts based on the right-side button UI"),
              div("4. When finished, you can save the images by pressing 'Save Results' button"),
              div("5. After saving, we will fetch you more data into the session"),
              br(),
              h2("References:"),
              tags$a(href="https://github.com/Sage-Bionetworks/mhealthannotator",
                     "Link to GitHub Package")
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
