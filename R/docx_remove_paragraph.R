#' Remove a paragraph found from a regular expression
#'
#' @param r an officer::rdocx object
#' @param location_regex The regex to search for (for removing the paragraph)
#'
#' @export
docx_remove_paragraph <- function(r, location_regex) {
  doc_obj = r$doc_obj$get()

  nodes = xml2::xml_find_all(doc_obj, ".//w:t")
  nodes_with_regex = which(grepl(location_regex,nodes))

  xml2::xml_remove(nodes[nodes_with_regex])

  return(x)
}
