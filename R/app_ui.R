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
        text = 'Dashboards',
        tabName = 'recon_vis',
        startExpanded = TRUE,
        menuSubItem(
          text="Progress",
          tabName="progress",
          icon = shiny::icon("angle-right")
        ),
        menuSubItem(
          text="Anomalies",
          tabName="anomalies",
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
      )
    )
  )
  body <- dashboardBody(
    use_waiter(),
    waiter_show_on_load(color = "#002244"),

    golem_add_external_resources(),

    tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #003366;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #006699;
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #ffffff;
                                }

                                .box-header h3.box-title {
                                  font-weight: bold;
                                  font-size: 24px;
                                }

                                '))),


    tabItems(
      tabItem(tabName = "about",
              h1("Recon Monitoring Dashboard"),
              div("Property of DataBrew LLC."),
              div("@Author: atediarjo@gmail.com"),
              div("@Reviewed by : joe@brew.cc, xing@brew.cc"),
              br(),
              h2("About"),
              div("This dashboard contains all the components to monitor Kwale survey collection process."),
              div("Which covers data related to progress, internet connectivity, fieldworkers, and identified anomalies"),
              br(),
              h2("References"),
              div("GitHub Repository: https://github.com/databrew/recon-monitoring")
      ),
      tabItem(
        tabName="progress",
        fluidPage(
          fluidRow(
            mod_progress_ui("household")
          )
        )
      ),
      tabItem(
        tabName="anomalies",
        fluidPage(
          fluidRow(
            mod_anomalies_ui("anomalies")
          )
        )
      ),
      tabItem(
        tabName="internet_coverage",
        fluidPage(
          fluidRow(
            mod_internet_coverage_ui("household")
          )
        )
      ),
      tabItem(
        tabName="fieldworker_performance",
        fluidPage(
          fluidRow(
            mod_fieldworker_performance_ui("registration")
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
