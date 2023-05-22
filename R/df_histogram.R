#' Quick histogram
#'
#' @param .data A data frame containing the column `.col`
#' @param .col The column for which to create a histogram
#'
#' @export
df_histogram <- function(.data, .col) {
  .col = rlang::enquo(.col)

  plotly::plot_ly(
    x = .data %>% pull(!!.col),
    type='histogram'
  )
}
