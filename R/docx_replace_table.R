#' Replace a table within an officer::rdocx object
#'
#' @param r An rdocx object
#' @param df A data frame to be inserted
#' @param caption_text The location to insert the table. See @details.
#' @param node_details Optional. A data frame returned from `node_details()`. If `NULL`, it will be
#' generated.
#'
#' @details The index of the table is discovered by looking for the captions beyond the Table of Contents on
#' the document. The `caption_text` is searched (via regex), and a table is searched for between the following
#' nodes and the next caption. If exactly one is found, it is replaced with df. Otherwise, an error is
#' thrown.
#'
#' Table properties from the original table are kept.
#'
#' @export
#'
docx_replace_table <- function(r, df, caption_text, node_details = NULL) {
  if(is.null(node_details)) node_details = docx_node_details(r)

  # Find a caption with the table name
  node_details = node_details %>%
    mutate(tbl_caption = node_detect(node, caption_text) & after_toc)

  captions = which(node_details$toc_ptr)
  tbl_caption_node_index = which(node_details$tbl_caption)
  next_caption_after_tbl_caption = min(captions[captions > tbl_caption_node_index])

  # The target table should be between the caption and the next caption; there should only be one
  tbls = which(node_details$is_table)
  next_tbl = min(tbls[tbls > tbl_caption_node_index & tbls < next_caption_after_tbl_caption])
  if(length(next_tbl) != 1) rlang::abort('More than one/No table found in section')

  xml_doc = docx_xml(r)
  xml_tbl_new = table_to_node(df)

  # For some reason, you need to explicitly spell out each step
  xml_doc_children = xml_children(xml_children(xml_doc))
  xml_tbl_old = xml_doc_children[[next_tbl]]
  xml_tbl_old_children = xml_children(xml_tbl_old)
  xml_tbl_old_prop = xml_tbl_old_children[[1]]
  xml_tbl_new_children = xml_children(xml_tbl_new)
  xml_tbl_new_prop = xml_tbl_new_children[[1]]

  # Put the old properties on the new table properties
  xml_replace(
    xml_tbl_new_prop,
    xml_tbl_old_prop
  )

  # Put the new table on the old doc
  xml_replace(
    xml_tbl_old,
    xml_tbl_new
  )

  return(invisible())
}
