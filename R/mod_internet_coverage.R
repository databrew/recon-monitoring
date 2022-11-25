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
          width = NULL, status = "primary", solidHeader = TRUE)),
        column(6, box(
          title = 'Time Lag Distribution by Ward',
          plotlyOutput(ns('time_lag_dist_by_ward'), height = 400),
          width = NULL, status = "primary", solidHeader = TRUE))
      ),
      fluidRow(
        column(6, box(
          title = 'Time Lag Raw Table',
          DT::dataTableOutput(ns("time_lag_table"), height = 400),
          width = NULL, status = "primary", solidHeader = TRUE)),
        column(6, box(
          title = 'Lag Map',
          leafletOutput(ns('lag_map_plot'), height = 400),
          width = NULL, status = "primary", solidHeader = TRUE))
      )
    )


  )
}

#' internet_coverage Server Functions
#'
#' @noRd
mod_internet_coverage_server <- function(input, output, session){
    ns <- session$ns
    filename <- tempfile(fileext = '.csv')
    data <-  get_s3_data(s3obj = paws::s3(),
                         bucket = 'databrew.org',
                         object_key = "kwale/clean-form/reconbhouseholdtraining/reconbhouseholdtraining.csv",
                         filename = filename) %>%
      read.csv(.) %>%
      tibble::as_tibble(.name_repair = "unique") %>%
      dplyr::mutate(ward = ifelse(ward == "", "N/A", ward),
                    community_health_unit = ifelse(community_health_unit == "", "N/A", community_health_unit),
                    village = ifelse(village == "", "N/A", village),
                    lag = as.numeric(ymd_hms(SubmissionDate)- ymd_hms(end_time)))

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
          scale_fill_brewer(palette="Accent") +
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
      data <- values$orig_data %>%
        as_tibble(.name_repair = "universal") %>%
        dplyr::filter(!is.na(Latitude)) %>%
        dplyr::select(ward, Latitude, Longitude, lag)

      if(nrow(data) > 0){
        mapv <- mapview(
          data,
          xcol = "Longitude",
          ycol="Latitude",
          zcol = "lag",
          crs = 4269,
          grid = FALSE)

        map_plot <- mapv + mapview(NULL,
                                   crs = 4269,
                                   grid = FALSE)

        map_plot@map

      }else{
        mapview(NULL,
                crs = 4269,
                grid = FALSE)
      }
    })
}

## To be copied in the UI
# mod_internet_coverage_ui("internet_coverage_1")

## To be copied in the server
# mod_internet_coverage_server("internet_coverage_1")
