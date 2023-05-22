#' Set the formatting to indent a table node when it's the first column and it starts with two spaces
#'
#' @export
tablenode_indent <- function(t) {
  first_col = xml2::xml_find_all(t, '//w:tbl/w:tr/w:tc[1]')
  first_col_text = xml2::xml_text(first_col)
  mult_indents = which_grepl(first_col_text, '^     ', ret='all')
  if(length(mult_indents) > 0) {
    rlang::inform(str_glue('Row(s) {str_c(mult_indents, collapse = ", ")} look to have multiple indents (not handled)'))
  }
  first_col_indented = first_col[which_grepl(first_col_text, '^  ', ret='all')]
  first_col_indented_style = xml2::xml_find_all(first_col_indented, 'w:p/w:pPr/w:pStyle')
  xml2::xml_set_attr(first_col_indented_style, "w:stlname", "C-Table Text Indented")
  node_remove_spacing(t)
  return(t)
}
