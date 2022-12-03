#' @description function to get household data from s3
#' @import paws
#' @import dplyr
#' @return tibble dataframe
get_object_update_time <- function(){
  s3obj <- paws::s3()
  key_list <-list(
    registration = "kwale/clean-form/reconaregistration/reconaregistration.csv",
    household = "kwale/clean-form/reconbhousehold/reconbhousehold.csv",
    anomalies = "kwale/anomalies/anomalies.csv"
  )
  purrr::map_dfr(
    names(key_list),
    function(key){
      object <- s3obj$get_object(
        Bucket = "databrew.org", Key = key_list[key])
      tribble(
        ~data_type,
        ~last_updated_UTC,
        key,
        object$LastModified) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          last_updated_EAT = with_tz(
            last_updated_UTC,
            "Africa/Addis_Ababa")) %>%
        dplyr::mutate_all(as.character)
    }
  )
}
