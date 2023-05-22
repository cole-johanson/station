#' @describeIn docx_body_nodeset Get the xml2::xml_document object from an officer::rdocx object
#'
#' @param r An officer::rdocx object
#'
#' @export
docx_xml <- function(r) {
  r$doc_obj$get()
}
