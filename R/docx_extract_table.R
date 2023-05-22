#' Extract a table from a officer::rdocx object
#'
#' @export
docx_extract_table <- function(r, table_num = 1) {
  # From https://ardata-fr.github.io/officeverse/extract-content.html
  content <- officer::docx_summary(r)
  table_cells <- subset(content, content_type %in% "table cell")
  table_body <- subset(table_cells, !is_header)
  table_body <- table_body[,c("row_id", "cell_id", "text")]
  df = tapply(
      table_body$text,
      list(
        row_id = table_body$row_id,
        cell_id = table_body$cell_id
      ),
      FUN = I
    ) %>%
    tibble::as_tibble() %>%
    table_use_first_row_as_names() %>%
    table_drop_blank_rows() %>%
    mutate_all(str_trim)
  return(df)
}

#' Use the first row as column names for a data frame
#'
#' @export
table_use_first_row_as_names <- function(df) {
  colnames(df) = df[1,]
  df = df %>%
    slice(-1)
}

#' Remove blank rows from a table
#'
#' @export
table_drop_blank_rows <- function(df) {
  is_blank <- function(x) {x == "" | is.na(x)}
  # Thanks ChatGPT
  all_na_rows <- apply(is_blank(df), 1, all)
  df <- df[!all_na_rows, ]
  return(df)
}
