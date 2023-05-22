#' Insert figure in a docx
#'
#' @export
docx_insert_figure <- function(r, file_name, header_text, header_num) {
  fig_path = rtf_extract_figure(file_name)

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

  for(i in 1:length(fig_path)) {
    xml2::xml_add_child(
      xml2::xml_find_first(r$doc_obj$get(), "/w:document/w:body"),
      figure_to_node(fig_path[i]),
      .where = idx_header_of_interest + i
    )
  }
}
