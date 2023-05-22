#' Get the nth biggest value
#'
#' @param x A vector
#' @param n An integer
#'
#' @export
v_which_max <- function(x, n = 1) {
  nth_biggest = sort(x, decreasing = T)[n]
  which(x >= nth_biggest)
}
