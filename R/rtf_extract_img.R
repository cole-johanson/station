#' Extract figure from rtf file and return its location
#'
#' For now, we assume there is exactly one media file
#'
#' @param file_name rtf file location
#' @param which_fig Which figure to pull (experimental)
#'
#' @export
rtf_extract_figure <- function(file_name, which_fig = NULL) {
  # Call pandoc, which will put it in the temp_dir()
  tmpdir = fs::path(tempdir(),file_remove_suffix(file_name))
  system(
    str_glue('pandoc "{file_name}" --extract-media {tmpdir}'),
    show.output.on.console = FALSE
  )
  files = system(
    str_glue('ls {tmpdir}'),
    intern = TRUE
  )

  if(!is.null(which_fig)) return(file.path(tmpdir,files[which_fig]))

  return(file.path(tmpdir,files))
}
