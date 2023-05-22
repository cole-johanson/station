#' As percent
#'
#' Convert a real number to a string percentage for output
#'
#' @param x A real number (typically between 0 and 1)
#' @param round An integer of how many digits to round to
#'
#' @return a string
#'
#' @export
as_percent <- function(x, round=0) {
  str_c(base::round(x*100,digits=round),'%')
}
