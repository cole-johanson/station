#' @export
pdf_get_length <- function(file) {
  purrr::map(file,~unlist(pdftools::pdf_length(.)))
}
