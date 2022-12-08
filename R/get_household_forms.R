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
  hh <- read.csv(filename, row.names = 1) %>%
    tibble::as_tibble(.name_repair = "unique") %>%
    mutate(wid = as.character(ifelse(is.na(wid_qr), wid_manual, wid_qr))) %>%
    dplyr::mutate(
      Latitude = as.numeric(Latitude),
      Longitude = as.numeric(Longitude),
      ward = ifelse(ward == "", "N/A", ward),
      community_health_unit = ifelse(community_health_unit == "", "N/A", community_health_unit),
      village = ifelse(village == "", "N/A", village),
      house_wall_material = ifelse(nchar(house_wall_material_other) > 0, house_wall_material_other, house_wall_material),
      roof_type = ifelse(nchar(roof_type_other) > 0, roof_type_other, roof_type)) %>%
    dplyr::select(
      wid,
      hh_id,
      ward,
      community_health_unit,
      village,
      Longitude,
      Latitude,
      SubmissionDate,
      end_time,
      # Additional variables useful for analysis (so we can use this functionality for both analyses, reports, and dashboard)
      house_wall_material,
      roof_type,
      num_hh_members,
      num_hh_members_lt_5,
      num_hh_members_gt_15,
      num_hh_members_bt_5_15 = X15
      )
  return(hh)
}
