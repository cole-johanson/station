#' Insert table into docx
#'
#' @param r an officer::rdocx object
#' @param df a data frame
#' @param header_text The regex to search for (for inserting the text)
#' @param header_num The header number
#' @param table_title If not `NULL`, insert a table title above the table
#'
#' @export
docx_insert_table <- function(r,
                              df,
                              header_text = NULL,
                              header_num = NULL,
                              table_title = NULL,
                              align_table = NULL,
                              source_text = NULL,
                              footnote_text = NULL,
                              node_details = NULL,
                              toc_tlf_num = NULL
                              ) {
  if(is.null(header_text)) {
    # Insert at end of doc... which I believe is the default
    docx_insert_table_at_end(
      r=r,
      df=df,
      table_title=table_title,
      align_table=align_table,
      source_text=source_text,
      footnote_text=footnote_text
    )
    return()
  }

  if(is.null(node_details)) node_details = docx_node_details(r)

  table_text = if_else(!is.null(table_title), str_replace(table_title, '.*[0-9](.*)', '\\1') %>% str_trim(), NA_character_)

  node_details = node_details %>% mutate(
    has_header_text = node_detect(node, header_text),
    is_header_text = node_text_l(node) == normal_text(header_text),
    has_header_num = node_detect(node, header_num, fixed=T),
    has_table_text = node_detect(node, table_text, fixed=T),
    has_toc_num = node_detect(node, toc_tlf_num, fixed=T),
    is_caption = node_is_caption(node)
  )

  idx_header_of_interest = which(
    node_details %>% mutate(
      x = after_toc & (has_table_text & has_toc_num) & is_caption
    ) %>% pull(x)
  )

  ## Temp: want to explain why we can't insert into the table text
  idx_header_of_interest_tmp = which(
    node_details %>% mutate(
      x = (has_header_text | has_header_num) & after_toc & (is_header | has_header_num)
    ) %>% pull(x)
  )
  if(length(idx_header_of_interest_tmp) > 1) {
    print('Multiple matching captions found. Inserting into first.')
  }

  if(length(idx_header_of_interest) == 0) {
    rlang::abort('No index found with the TOC header.')
  }

  if(length(idx_header_of_interest) > 1) {
    rlang::warn('Multiple matching captions found. Inserting into first.')
    idx_header_of_interest = idx_header_of_interest[1]
  }

  if(!is.null(table_title)) {
    xml2::xml_add_child(
      xml2::xml_find_first(r$doc_obj$get(), "/w:document/w:body"),
      text_to_node(table_title, bold=T),
      .where = idx_header_of_interest
    )
  }

  # Add the table(s)
  for(i in 1:length(df)) {
    xml2::xml_add_child(
      xml2::xml_find_first(r$doc_obj$get(), "/w:document/w:body"),
      table_to_node(df[[i]], align_table=align_table),
      .where = idx_header_of_interest + i*2-1
    )
    if(i != length(df)) {
      xml2::xml_add_child(
        xml2::xml_find_first(r$doc_obj$get(), "/w:document/w:body"),
        text_to_node('  '),
        .where = idx_header_of_interest + i*2
      )
    }
  }

  # Add the source (and remove existing?)
  if(!is.null(source_text)) {
    xml2::xml_add_child(
      xml2::xml_find_first(r$doc_obj$get(), "/w:document/w:body"),
      text_to_node(source_text, r = r),
      .where = idx_header_of_interest + length(df)*2
    )
  }

  # Add the footnotes
  if(!is.null(footnote_text)) {
    footnote_text_split = stringr::str_split_1(footnote_text, '\n')
    footnote_text_split = footnote_text_split[footnote_text_split != ""]
    for(j in 1:length(footnote_text_split)) {
      xml2::xml_add_child(
        xml2::xml_find_first(r$doc_obj$get(), "/w:document/w:body"),
        text_to_node(footnote_text_split[j], r = r),
        .where = idx_header_of_interest + length(df)*2 + j
      )
    }
  }
}

#' @export
docx_insert_table_at_end <- function(r,
                                     df,
                                     table_title = NULL,
                                     align_table = NULL,
                                     source_text = NULL,
                                     footnote_text = NULL) {
  if(!is.null(table_title)) {
    xml2::xml_add_child(
      xml2::xml_find_first(r$doc_obj$get(), "/w:document/w:body"),
      text_to_node(table_title, bold=T)
    )
  }

  # Add the table(s)
  for(i in 1:length(df)) {
    xml2::xml_add_child(
      xml2::xml_find_first(r$doc_obj$get(), "/w:document/w:body"),
      table_to_node(df[[i]], align_table=align_table)
    )
    if(i != length(df)) {
      xml2::xml_add_child(
        xml2::xml_find_first(r$doc_obj$get(), "/w:document/w:body"),
        text_to_node('  ')
      )
    }
  }

  # Add the source (and remove existing?)
  if(!is.null(source_text)) {
    xml2::xml_add_child(
      xml2::xml_find_first(r$doc_obj$get(), "/w:document/w:body"),
      text_to_node(source_text, r = r)
    )
  }

  # Add the footnotes
  if(!is.null(footnote_text)) {
    footnote_text_split = stringr::str_split_1(footnote_text, '\n')
    footnote_text_split = footnote_text_split[footnote_text_split != ""]
    for(j in 1:length(footnote_text_split)) {
      xml2::xml_add_child(
        xml2::xml_find_first(r$doc_obj$get(), "/w:document/w:body"),
        text_to_node(footnote_text_split[j], r = r)
      )
    }
  }

  xml2::xml_add_child(
    xml2::xml_find_first(r$doc_obj$get(), "/w:document/w:body"),
    text_to_node('  ')
  )
}
