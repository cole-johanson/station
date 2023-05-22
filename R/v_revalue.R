#' Revalue a vector
#'
#' Replace values in a vector
#'
#' @param x a Vector
#' @param r a named vector where the names are what to replace the values with
#'
#' @examples
#' revalue(x = c("a","b","c"), r = c("x"="a", "y"="b"))
#'
#' @export
v_revalue <- function(x, r) {
  for(i in 1:length(r)) {
    x = replace(x, x==r[i], names(r)[i])
  }
  return(x)
}
