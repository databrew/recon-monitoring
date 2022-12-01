#' @description function to get household data from s3
#' @import paws
#' @import dplyr
#' @return tibble dataframe
#' @export
get_registartion_forms <- function(){
  s3obj = paws::s3()
  filename <- tempfile(fileext = '.csv')
  s3obj$download_file(
    Bucket= 'databrew.org',
    Key = "kwale/clean-form/reconaregistration/reconaregistration.csv",
    Filename = filename)

  registration <- read.csv(filename, row.names = 1) %>%
    tibble::as_tibble(.name_repair = "unique") %>%
    dplyr::mutate(wid = as.character(wid),
                  wid_cha = as.character(
                    ifelse(is.na(cha_wid_qr),
                           cha_wid_manual,
                           cha_wid_qr)),
                  Latitude = as.numeric(Latitude),
                  Longitude = as.numeric(Longitude))
  return(registration)
}
