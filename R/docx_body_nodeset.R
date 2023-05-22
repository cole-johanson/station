#' Get the body nodeset of an rdocx or rdocx XML element
#'
#' @param x An officer::rdocx or an xml2::xml_document object
#'
#' @export
docx_body_nodeset <- function (x) {
  UseMethod("docx_body_nodeset", x)
}

#' @rdname docx_body_nodeset
#' @export
docx_body_nodeset.rdocx <- function(x) {
  docx_body_nodeset(docx_xml(x))
}

#' @rdname docx_body_nodeset
#' @export
docx_body_nodeset.xml_document <- function(x) {
  xml_find_all(x,"/w:document/w:body/*")
}
