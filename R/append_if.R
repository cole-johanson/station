#' Conditionally append values to a list or vector
#'
#' @export
append_if <- function(l, condition, ...) {
  UseMethod("append_if", l)
}

#' @export
append_if.default <- function(l, condition, ...) {
  if(condition) {
    return(c(l, c(...)))
  } else {
    return(l)
  }
}

#' @export
append_if.list <- function(l, condition, ...) {
  if(condition) {
    return(c(l, list(...)))
  } else {
    return(l)
  }
}
