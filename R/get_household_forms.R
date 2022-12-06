#' @description function to get household data from s3
#' @import paws
#' @import dplyr
#' @return tibble dataframe
#' @export
get_household_forms <- function(){
  s3obj = paws::s3()
  filename <- tempfile(fileext = '.csv')
  s3obj$download_file(
    Bucket= 'databrew.org',
    Key = "kwale/clean-form/reconbhousehold/reconbhousehold.csv",
    Filename = filename)
  hh <- fread(filename) %>%
    tibble::as_tibble(.name_repair = "unique") %>%
    tidyr::drop_na(wid_qr) %>%
    dplyr::mutate(
      wid = as.character(wid_qr),
      Latitude = as.numeric(Latitude),
      Longitude = as.numeric(Longitude),
      ward = ifelse(ward == "", "N/A", ward),
      community_health_unit = ifelse(community_health_unit == "", "N/A", community_health_unit),
      village = ifelse(village == "", "N/A", village)) %>%
    dplyr::select(
      wid,
      hh_id,
      ward,
      community_health_unit,
      village,
      Longitude,
      Latitude,
      SubmissionDate,
      end_time)
  return(hh)
}
