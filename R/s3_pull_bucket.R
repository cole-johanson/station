#' List S3 bucket contents
#'
#' @param bucket The bucket on S3 to pull contents of
#' @param local_data_directory The local directory to pull the bucket to
#' @param file_regex The regex to apply to subset the file list
#'
#' @export
#'
s3_pull_bucket <- function(bucket, local_data_directory, file_regex = '.*\\.rtf$') {
  files = s3_list_bucket_contents(bucket=bucket, file_regex=file_regex)
  purrr::map(files, s3_pull_file, bucket=bucket, local_data_directory=local_data_directory)
}
