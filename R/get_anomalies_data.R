#' Function to get anomalies data from s3
#' @import paws
#' @import dplyr
#' @return tibble dataframe
#' @export
get_anomalies_data <- function(){
  bucket_name <- 'databrew.org'
  s3obj <- paws::s3()
  filename <- tempfile(fileext = '.csv')
  bucket_key <- s3obj$list_objects_v2(
    Bucket = bucket_name,
    Prefix = 'kwale/anomalies/anomalies-identification-history/run_date') %>%
    .$Contents %>%
    tail(1) %>%
    .[[1]] %>%
    .$Key


  s3obj$download_file(
    Bucket= bucket_name,
    Key = bucket_key,
    Filename = filename)

  anomalies <- fread(filename) %>%
    tibble::as_tibble(.name_repair = "unique")
  return(anomalies)
}
