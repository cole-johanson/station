#' Convert files to PDFs
#'
#' Convert local files to PDFs (in the same directory)
#'
#' @param file a single string or a vector of strings representing file names
#'
#' @export
rtf_convert_to_pdf_here <- function(file) {
  convert <- function(file) {
    cmd_line = str_glue(
      '"{get_golem_config(\"libreoffice_loc\")}"',
      '--convert-to pdf',
      '--outdir {file_get_directory_path(file)}',
      '{file}',
      .sep = " "
    )
    system(cmd_line)
  }
  purrr::map(file, convert)

  return(invisible())
}
