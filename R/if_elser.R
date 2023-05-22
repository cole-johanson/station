#' If/Else statement on one line
#'
#' @param test A logical
#' @param yes Output if `test` is TRUE
#' @param no Output if `test` is FALSE
#'
#' @export
if_elser <- function(test, yes, no) {
  if(test) return(yes)
  return(no)
}
