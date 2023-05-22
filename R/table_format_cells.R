#' Remove spaces in parentheses throughout a table
#'
#' Many of the RTF files have formatting like '  21 ( 0.0)'. This function removes leading spaces within
#' the parentheses in every cell: '  21 (0.0)'
#'
#' @param df A data frame
#'
#' @export
table_format_cells <- function(df) {
  df %>%
    # Remove any spaces within the parentheses: 1 (  2.5%) -> 1 (2.5%)
    dplyr::mutate_all(stringr::str_replace_all,
      pattern = '\\([ ]*', replacement = '('
    ) %>%
    # Add a space between a number and a beginning parenthesis: 1(2.5%) -> 1 (2.5%)
    dplyr::mutate_all(stringr::str_replace_all,
      pattern = '([0-9])\\(', replacement = '\\1 \\('
    ) %>%
    # 0 (0.0%) -> 0
    dplyr::mutate_all(stringr::str_replace_all,
      pattern = '^\\s*0\\s*\\(.*\\%\\s*\\)', replacement = '0'
    ) %>%
    # 100.0% -> 100%
    dplyr::mutate_all(stringr::str_replace_all,
      pattern = '([^0-9])100\\.0%', replacement = '\\1100%'
    ) %>%
    # Remove all percentages when formatted as expected
    dplyr::mutate_all(stringr::str_replace_all,
      pattern = '([0-9]+ *\\([0-9\\.]+)%\\)', replacement = '\\1)'
    )
}

