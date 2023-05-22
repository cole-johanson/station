#' @describeIn s3_push_local_file
#'
#' @param files A list of files to push
#'
#' @export
#'
s3_push_local_files <- function(files = list.files(local_data_directory, recursive=T), local_data_directory) {
  purrr::map(files, s3_push_local_file)
}
