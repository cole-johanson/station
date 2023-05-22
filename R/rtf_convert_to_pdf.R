#' Convert files to PDFs
#'
#' Convert local files to PDFs (in the same directory)
#'
#' @param file a single string or a vector of strings representing file names
#'
#' @export
rtf_convert_to_pdf <- function(file) {
  convert <- function(file) {
    r2rtf:::rtf_convert_format(file, output_dir = file_get_directory_path(file), format='pdf')
  }
  purrr::map(file, convert)
}
