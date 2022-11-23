#' Utility function to retrieve Kwale filepath
#' @import paws
#' @import config
#' @import glue
get_s3_data <- function(s3obj = NULL, bucket, object_key, filename){
  svc$download_file(
    Bucket= bucket,
    Key = object_key,
    Filename = filename)
  return(filename)
}
