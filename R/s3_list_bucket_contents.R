#' List S3 bucket contents
#'
#' @param bucket The bucket on S3 to list contents of
#' @param file_regex The regex to apply to subset the file list
#'
#' @export
#'
s3_list_bucket_contents  <- function(bucket, file_regex = '.*\\.rtf$') {
  files_df = system(str_glue("aws s3api list-objects --bucket {bucket}"), intern=T) %>% jsonlite::fromJSON()

  return(str_subset(unique(files_df$Contents$Key), file_regex))
}
