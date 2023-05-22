#' Indent string
#'
#' Use in conjunction with DT with `escape = FALSE`
#'
#' @param x a string
#'
#' @details https://github.com/rstudio/DT/issues/215
#'
#' @export
str_indent_DT <- function(x) {
  str_c('&nbsp;',x)
}
