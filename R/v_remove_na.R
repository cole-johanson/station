#' Remove NA values
#'
#' @param x a vector
#'
#' @return a vector
#'
#' @export
v_remove_na <- function(x) {
  x[!is.na(x)]
}
