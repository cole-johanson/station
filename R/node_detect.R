#' Test whether a node matches a pattern in its text
#'
#' @param n A node or nodeset
#' @param text Text to search for. See @details for more information
#' @param fixed Logical. See [grepl()]
#' @param ignore.case Logical. See [grepl()]
#'
#' @details
#' The search is conducted only on the alphanumeric characters. Any non-alphanumeric characters are replaced
#' with an any-character '.' search. This is to correct for some seemingly odd characters in the template
#' we see for 877, e.g. 'Figure.1' works, but 'Figure 1' does not.
#'
#' @export
node_detect <- function(n, text, fixed=F, ignore.case=F) {
  if(!fixed) {
    text = stringr::str_replace_all(text,'[^A-Za-z0-9]','.') %>% # Use only alphanumeric
      stringr::str_replace_all('\\s+',' ') # Replace all whitespace with a single space
  }

  grepl(text, lapply(n, node_text, fixed=fixed), fixed=fixed, ignore.case=ignore.case)
}

#' @export
node_text <- function(n, fixed=F) {
  text = n %>% xml_find_all('w:r/w:t') %>% xml2::xml_text() %>% str_c(collapse="")

  if(!fixed) {
    text = text %>%
      stringr::str_replace_all('\\s+',' ') %>% # Replace all whitespace with a single space
      stringr::str_replace_all('[^A-Za-z0-9]','.') # Use only alphanumeric
  }

  return(text)
}

#' @export
node_text_l <- function(n, fixed=F) {
  text = unlist(purrr::map(n, ~xml_find_all(.,'w:r/w:t') %>% xml2::xml_text() %>% str_c(collapse="")))

  if(!fixed) {
    text = text %>%
      stringr::str_trim() %>%
      stringr::str_replace_all('\\s+',' ') %>% # Replace all whitespace with a single space
      stringr::str_replace_all('[^A-Za-z0-9]','.') # Use only alphanumeric
  }

  return(text)
}

#' @export
normal_text <- function(s) {
  s %>%
    stringr::str_trim() %>%
    stringr::str_replace_all('\\s+',' ') %>% # Replace all whitespace with a single space
    stringr::str_replace_all('[^A-Za-z0-9]','.') # Use only alphanumeric
}
