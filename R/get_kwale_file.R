#' Utility function to retrieve Kwale filepath
#' @import paws
#' @import config
#' @import glue
get_kwale_file <- function(type){
  config_path <- file.path(
    golem::get_golem_options("config"))
  cf <- config::get(file = config_path)
  svc <- paws::s3()
  filepath <- tempfile(fileext = '.csv')
  svc$download_file(
    Bucket = cf$s3$bucket_name,
    Key = cf$s3$keys[[type]],
    Filename = filepath)
  return(read.csv(filepath))
}
