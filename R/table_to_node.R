#' Convert a data frame to a Microsoft Word table
#'
#' @param df A data frame
#' @param ... Parameters passed to `officer` functions
#'
#' @export
table_to_node <- function(df, preserve_spacing=F, indent_col1=T, style = 'CSR-Table',
                          pos = "after", header = TRUE,
                          alignment = NULL, align_table = "center",
                          stylenames = officer::table_stylenames(rlang::set_names(rep("C-Table Text",ncol(df)),nm=colnames(df))),
                          first_row = TRUE, first_column = TRUE,
                          last_row = FALSE, last_column = FALSE,
                          no_hband = FALSE, no_vband = TRUE,
                          escape_cells = TRUE) {
  pt <- officer::prop_table(
    style = style, layout = officer::table_layout(),
    width = officer::table_width(), stylenames = stylenames,
    tcf = officer::table_conditional_formatting(
      first_row = first_row, first_column = first_column,
      last_row = last_row, last_column = last_column,
      no_hband = no_hband, no_vband = no_vband
    ),
    align = align_table
  )

  bt <- officer::block_table(x = df, header = header, properties = pt, alignment = alignment)
  x = xml2::as_xml_document(officer::to_wml(bt, add_ns = TRUE, escape_cells = escape_cells))
  if(indent_col1) {
    x = tablenode_indent(x)
  }

  # Handle sups
  cells = x %>% xml2::xml_find_all('*/w:tc')
  cell_text = cells %>% xml2::xml_text()
  cells_with_sups = cells[which(grepl('<sup>.</sup>',cell_text))]
  if(length(cells_with_sups) > 0) {
    cells_with_sups_suptext = stringr::str_match(xml2::xml_text(cells_with_sups),'<sup>(?<suptext>.)</sup>')[,"suptext"]
    cells_with_sups_nonsuptext = stringr::str_match(xml2::xml_text(cells_with_sups),'(?<nonsuptext>.*)<sup>(.)</sup>')[,"nonsuptext"]
    xml2::xml_set_text(cells_with_sups, cells_with_sups_nonsuptext)
    cells_with_sups_r = cells_with_sups %>% xml2::xml_find_all('w:p/w:r')
    # Add a run
    wp_ns_yes = "<w:p xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\" xmlns:wp=\"http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\" xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\">"
    for(i in 1:length(cells_with_sups)) {
      run_to_add = suppressWarnings(xml2::read_xml(str_glue(
        '<w:r><w:rPr><w:vertAlign w:val="superscript"/></w:rPr><w:t>{cells_with_sups_suptext[i]}</w:t></w:r>'
      ), ns = wp_ns_yes))
      cell_of_interest = cells_with_sups_r[i]
      cell_of_interest = cell_of_interest %>% xml2::xml_add_sibling(run_to_add)
    }
  }

  if(preserve_spacing) {
    node_preserve_spacing(x)
  }
  return(x)
}

