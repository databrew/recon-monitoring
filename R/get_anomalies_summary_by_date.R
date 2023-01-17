#' Function to get household data from s3
#' @import paws
#' @import dplyr
#' @import lubridate
#' @return tibble dataframe
#' @export
get_anomalies_summary_by_date <- function(){
  s3obj = paws::s3()
  filename <- tempfile(fileext = '.csv')
  s3obj$download_file(
    Bucket= 'databrew.org',
    Key = "kwale/anomalies/anomalies-identification-history-summary/anomalies-identification-history-summary.csv",
    Filename = filename)
  fread(filename) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(run_date = lubridate::as_date(run_date))
}
