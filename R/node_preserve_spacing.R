#' Reserve spacing
#'
#' @param x An xml node
#'
#' @export
node_preserve_spacing <- function(x) {
  t_vals = xml2::xml_find_all(x, '//w:t')
  xml2::xml_set_attrs(t_vals, c("xml:space"="preserve"))
}

#' @export
node_remove_spacing <- function(x) {
  t_vals = xml2::xml_find_all(x, '//w:t')
  xml2::xml_set_attrs(t_vals, c("xml:space"="default"))
}
