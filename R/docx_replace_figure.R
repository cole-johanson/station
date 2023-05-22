#' Insert figure into a docx
#'
#' @param r an officer::rdocx object
#' @param fig_path The relative path of the figure to insert
#' @param caption_text The text to search for (for inserting the figure)
#'
#' #' @details The index of the figure is discovered by looking for the captions beyond the Table of Contents on
#' the document. The `caption_text` is searched (via regex), and a table is searched for between the following
#' nodes and the next caption. If exactly one is found, it is replaced with df. Otherwise, an error is
#' thrown.
#'
#' @return an altered rdocx object
#'
#' @export
docx_replace_figure <- function(r, fig_path, caption_text, style = NULL, node_details = NULL) {
  if(is.null(node_details)) node_details = docx_node_details(r)

  # Find a caption with the caption_text
  node_details = node_details %>%
    mutate(fig_caption = node_detect(node, caption_text) & after_toc & is_caption)

  captions = which(node_details$toc_ptr)
  fig_caption_node_index = which(node_details$fig_caption)
  next_caption_after_fig_caption = min(captions[captions > fig_caption_node_index])

  # The target table should be between the caption and the next caption; there should only be one
  figs = which(node_details$is_figure)
  next_fig = min(figs[figs > fig_caption_node_index & figs < next_caption_after_fig_caption])
  if(length(next_fig) != 1) rlang::abort('More than one/No table found in section')

  xml_doc = docx_xml(r)
  xml_fig_new = figure_to_node(fig_path)

  # For some reason, you need to explicitly spell out each step
  xml_doc_children = xml_children(xml_children(xml_doc))
  xml_fig_old = xml_doc_children[[next_fig]]
  xml_fig_old_children = xml_children(xml_fig_old)
  xml_fig_old_prop = xml_fig_old_children[[1]]
  xml_fig_new_children = xml_children(xml_fig_new)
  xml_fig_new_prop = xml_fig_new_children[[1]]

  # Put the old properties on the new table properties
  xml_replace(
    xml_fig_new_prop,
    xml_fig_old_prop
  )

  # Put the new table on the old doc
  xml_replace(
    xml_fig_old,
    xml_fig_new
  )

  return(invisible())
}
