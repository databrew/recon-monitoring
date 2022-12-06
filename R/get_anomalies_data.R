#' @description function to get anomalies data from s3
#' @import paws
#' @import dplyr
#' @return tibble dataframe
#' @export
get_anomalies_data <- function(){
  s3obj <- paws::s3()
  filename <- tempfile(fileext = '.csv')
  s3obj$download_file(
    Bucket= 'databrew.org',
    Key = "kwale/anomalies/anomalies.csv",
    Filename = filename)

  anomalies <- fread(filename) %>%
    tibble::as_tibble(.name_repair = "unique")
  return(anomalies)
}
