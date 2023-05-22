#' Prefix string
#'
#' Used for piping: apply the following parameters prior to the first
#'
#' @param string a string
#' @param ... see \link[stringr]{str_c}
#' @param sep see \link[stringr]{str_c}
#'
#' @export
#'
str_prefix <- function(string, ..., sep= "") {
  stringr::str_c(..., string, sep=sep)
}
