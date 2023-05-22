#' Get the node details from an officer::rdocx or an xml2::xml_document object
#'
#' Extract a data frame of nodes from the body of an officer::rdocx or an xml2::xml_document object
#'
#' @param x An officer::rdocx object or an xml2::xml_document object
#'
docx_node_details <- function(x) {
  xml = docx_body_nodeset(x)

  t = tibble::tibble(
    node = purrr::map(xml, as.vector)
  ) %>%
    mutate(
      is_header1 = node_is_header(node, header_num = 1),
      is_header = node_is_header(node),
      toc = node_detect(node, "TABLE OF CONTENTS", ignore.case=T) & is_header1,
      is_caption = node_is_caption(node),
      toc_ptr = node_is_toc_pointer(node),
      is_table = node_is_table(node),
      is_figure = node_is_figure(node)
    )

  # Find the row which is the next header after "Table of contents"
  headers = which(t$is_header1)
  next_header_after_toc = min(headers[headers > which(t$toc)])

  t = t %>%
    mutate(
      after_toc = row_number() >= next_header_after_toc
    )

  return(t)
}

#' Determine whether an xml node is a header. Takes in a node or a nodeset, and returns T/F
#'
#' @param n A node or nodeset
#' @param header_num An integer if we want to filter to a header of a certain type
#'
#' @export
node_is_header <- function(n, header_num = NULL) {
  #xml2::xml_attr(n, "w:pPr/w:pStyle")
  #xml_attrs(xml_find_all(n, 'w:pPr/w:pStyle'))
  #purrr::map(n, ~xml_attrs(xml_find_all(., 'w:pPr/w:pStyle')))
  pattern = str_glue('Heading{ifelse(!is.null(header_num),header_num,"")}')
  node_is_style(n, pattern)
}

#' Determine whether an xml node is a caption.
#'
#' @param n A node or nodeset
#'
#' @export
node_is_caption <- function(n) {
  pattern = 'Caption'
  node_is_style(n, pattern)
}

#' Determine whether an xml node has a specified style.
#'
#' @param n A node or nodeset
#'
#' @export
node_is_style <- function(n, pattern) {
  unlist(purrr::map(
    n,
    ~any(grepl(pattern,xml_attrs(xml_find_all(., 'w:pPr/w:pStyle'))))
  ))
}

#' @export
node_is_toc_pointer <- function(n) {
  unlist(purrr::map(
    n,
    ~any(grepl('SEQ Table',xml_attrs(xml_find_all(., 'w:fldSimple'))))
  ))
}

#' Test if a node is a table
#'
#' @param n An xml node
#'
#' @export
node_is_table <- function(n) {
  unlist(purrr::map(
    n,
    ~length(xml_find_all(., 'w:tblPr')) > 0
  ))
}

#' Test if a node is a table
#'
#' @param n An xml node
#'
#' @export
node_is_figure <- function(n) {
  unlist(purrr::map(
    n,
    ~length(xml_find_all(., 'w:r/w:drawing')) > 0
  ))
}
