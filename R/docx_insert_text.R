#' Insert table into docx
#'
#' @param r an officer::rdocx object
#' @param df a data frame
#' @param header_text The regex to search for (for inserting the text)
#' @param header_num The header number
#' @param table_title If not `NULL`, insert a table title above the table
#'
#' @export
docx_insert_text <- function(r, text_to_insert, header_text, header_num, table_title = NULL, node_details = NULL, align_table = NULL, footnote_text = NULL) {
  if(is.null(node_details)) node_details = docx_node_details(r)

  node_details = node_details %>% mutate(
    has_header_text = node_detect(node, header_text),
    has_header_num = node_detect(node, header_num, fixed=T)
  )

  idx_header_of_interest = which(
    node_details %>% mutate(
      x = (has_header_text | has_header_num) & after_toc & (is_header | has_header_num)
    ) %>% pull(x)
  )

  xml2::xml_add_child(
    xml2::xml_find_first(r$doc_obj$get(), "/w:document/w:body"),
    text_to_node(text_to_insert),
    .where = idx_header_of_interest
  )
}
