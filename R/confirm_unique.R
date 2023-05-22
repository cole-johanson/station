#' Confirm unique
#'
#' Returns an error if not unique. Otherwise, returns the originating data frame
#'
#' @param .data a data frame to be tested
#' @param ... <[`tidy-select`][dplyr_tidy_select]> Columns representing the primary key of the data frame
#'
#' @export
df_confirm_unique <- function(.data, ...) {
  x = .data %>% dplyr::group_by(...) %>% summarise(rows_per_pk=n(), .groups='drop') %>% filter(rows_per_pk > 1)
  if(nrow(x)>0) {
    rlang::abort(str_glue('.data contains {sum(x$rows_per_pk)} duplicates'))
  }
  return(.data)
}
