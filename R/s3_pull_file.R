#' Pull S3 file
#'
#' @param object The object (file) to pull from S3
#' @param bucket The bucket on S3 to pull contents of
#' @param local_data_directory The local directory to pull the bucket to
#'
#' @export
#'
s3_pull_file <- function(object, bucket, local_data_directory) {
  # Create the directories if needed
  file_create_directory(fs::path(local_data_directory,object))

  system(
    str_glue("aws s3api get-object --bucket {bucket} --key {object} {local_data_directory}/{object}")
  )
}
