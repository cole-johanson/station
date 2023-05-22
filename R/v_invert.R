#' Invert a named vector
#'
#' Sets the names of the vector to the value, and the values as the names
#'
#' @param x A named vector
#'
#' @examples
#' v_invert(c("a" = "A", "b" = "B"))
#'
#' @export
v_invert <- function(x) {
  purrr::set_names(names(x), x)
}
