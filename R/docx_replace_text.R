#' Replace text within an rdocx
#'
#' @param r An officer::rdocx object
#' @param text_to_replace Text that should be replaced
#' @param new_text Text that should be inserted
#' @param node_details A node_details data frame from docx_node_details(). If `NULL`, this will be derived.
#' @param caption Document caption in which to search for the given text. If `NULL`, search the whole document.
#' @param instance Which instance of the text to replace. If `NULL`, replace all.
#'
#' @export
docx_replace_text <- function(r, new_text, text_to_replace, node_details = NULL) {
  if(is.null(node_details)) {
    node_details = docx_node_details(r)
  }

  node_details = node_details %>%
    mutate(
      contains_replacement_text = node_detect(node, text_to_replace)
    )

  which_node = which(node_details$contains_replacement_text)

  #collapse_runs((node_details %>% filter(contains_replacement_text) %>% pull(node))[[1]])

  xml_doc = docx_xml(r)
  xml_doc_children = xml_children(xml_children(xml_doc))

  xml_par_new_text = xml2::xml_text(xml_doc_children[[which_node]])
  xml_par_new = text_to_node(xml_par_new_text)

  # For some reason, you need to explicitly spell out each step
  xml_par_old = xml_doc_children[[which_node]]
  xml_par_old_children = xml_children(xml_par_old)
  xml_par_old_prop = xml_par_old_children[[1]]
  xml_par_new_children = xml_children(xml_par_new)
  xml_par_new_prop = xml_par_new_children[[1]]

  xml_replace(
    xml_par_new_prop,
    xml_par_old_prop
  )

  # Put the new table on the old doc
  xml_replace(
    xml_par_old,
    xml_par_new
  )

  officer::body_replace_all_text(r, text_to_replace, new_text)

  return(invisible())
}




