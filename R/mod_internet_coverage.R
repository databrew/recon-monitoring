#' internet_coverage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_internet_coverage_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(6, box(
          title = 'Time Lag Distribution',
          plotlyOutput(ns('time_lag_dist'), height = 400),
          width = NULL, solidHeader= TRUE, )),
        column(6, box(
          title = 'Time Lag Distribution by Ward',
          plotlyOutput(ns('time_lag_dist_by_ward'), height = 400),
          width = NULL, solidHeader= TRUE, ))
      ),
      fluidRow(
        column(6, box(
          title = 'Time Lag Raw Table',
          DT::dataTableOutput(ns("time_lag_table"), height = 400),
          width = NULL, solidHeader= TRUE, )),
        column(6, box(
          title = 'Lag Map',
          leafletOutput(ns('lag_map_plot'), height = 400),
          width = NULL, solidHeader= TRUE, ))
      )
    )


  )
}

#' internet_coverage Server Functions
#'
#' @noRd
mod_internet_coverage_server <- function(input, output, session){
    ns <- session$ns

    data <-  get_household_forms() %>%
      dplyr::mutate(lag = as.numeric(ymd_hms(SubmissionDate)- ymd_hms(end_time)))

    values <- reactiveValues(
      orig_data = data,
    )

    output$time_lag_dist <- renderPlotly({
      ggplotly(
        values$orig_data %>%
          dplyr::mutate() %>%
          ggplot(aes(lag)) +
          geom_density(adjust = 0.5, fill = 'darkblue', alpha = 0.5) +
          labs(x = "lag (seconds)", y = "") +
          theme_minimal()
      )
    })

    output$time_lag_dist_by_ward <- renderPlotly({
      ggplotly(
        values$orig_data %>%
          ggplot(aes(x = ward, y = lag, fill = ward)) +
          geom_boxplot(alpha = 0.8, colour = "grey50") +
          theme_minimal() +
          scale_fill_brewer(palette="Dark2") +
          labs(x = "", y = "lag (seconds)") +
          theme(legend.position = "none") +
          theme(axis.text.x = element_text(angle=45, hjust=1))
      )
    })

    output$time_lag_table = DT::renderDataTable({
      DT::datatable(values$orig_data %>%
                      dplyr::select(hh_id,
                                    SubmissionDate,
                                    end_time,
                                    lag,
                                    ward,
                                    community_health_unit,village),
                    extensions = 'Buttons',
                    options = list(
                      dom = 'Bfrtip',
                      searching = FALSE,
                      pageLength = 5,
                      scrollX=TRUE,
                      buttons = list(
                        list(extend = "csv", text = "Download Full Results", filename = "data",
                             exportOptions = list(
                               modifier = list(page = "all")
                             )
                        )
                      ),
                      fixedColumns = list(leftColumns = 2)
                    )
                  )
    })


    output$lag_map_plot <- renderLeaflet({
      content_placeholder <- paste0("Household ID: {hh_id}<br/>",
                                    "Lag: {lag} secs")
      data <- values$orig_data %>%
        as_tibble(.name_repair = "universal") %>%
        dplyr::filter(!is.na(Latitude)) %>%
        dplyr::mutate(Latitude = as.numeric(Latitude),
                      Longitude = as.numeric(Longitude),
                      content = glue::glue(content_placeholder))

      pal <- colorNumeric(palette = "RdBu", domain = c(0:max(data$lag)), reverse = TRUE)

      leaflet(data) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addCircleMarkers(
          lng=~Longitude,
          stroke = FALSE,
          fillOpacity = 0.4,
          radius = 7,
          color = ~pal(lag),
          lat=~Latitude,
          popup=~content)
    })
}

## To be copied in the UI
# mod_internet_coverage_ui("internet_coverage_1")

## To be copied in the server
# mod_internet_coverage_server("internet_coverage_1")
