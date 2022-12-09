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
#' @import reactable
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
          icon = shiny::icon("bars-progress")
        ),
        menuSubItem(
          text="Fieldworker Performance",
          tabName="fieldworker_performance",
          icon = shiny::icon("users")
        ),
        menuSubItem(
          text="Anomalies",
          tabName="anomalies",
          icon = shiny::icon("circle-exclamation")
        ),
        menuSubItem(
          text="Internet Coverage",
          tabName="internet_coverage",
          icon = shiny::icon("wifi")
        )
      )
    )
  )
  body <- dashboardBody(
    use_waiter(),
    waiter_show_on_load(color = "#002244"),

    golem_add_external_resources(),

    tags$head(tags$style(HTML('
                                /* logo */

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #006699;
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #FFFFFF;
                                }

                                .box-header h3.box-title {
                                  font-size: 24px;
                                }

                                .box-header h3 {
                                font-weight: bold;
                                }

                                .info-box .logo {
                                font-weight: bold;
                                font-size: 8px;
                                }

                                '))),


    tabItems(
      tabItem(
        tabName = "about",
        fluidPage(
          fluidRow(
            mod_landing_page_ui("landing")
          )
        )
      ),
      tabItem(
        autoWaiter(),
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
