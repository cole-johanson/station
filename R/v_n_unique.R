#' Get the number of unique values in a vector
#'
#' @param x A vector
#'
#' @export
v_n_unique <- function(x) {
  length(unique(x))
}
