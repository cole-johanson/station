#' Push a single file to S3
#'
#' @param file The local file to push. Note that the path on S3 will mimic the path from the `local_data_directory`
#' @param bucket The bucket to push it to
#' @param local_data_directory the local directory the file is located in.
#'
#' @export
#'
s3_push_local_file <- function(file, bucket, local_data_directory) {
  # aws.s3::put_object(file=file, object = file, bucket = bucket)
  system(str_glue("aws s3api put-object --bucket {bucket} --key {file} --body {local_data_directory}/{file}"))
}
