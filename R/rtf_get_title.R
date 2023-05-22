#' Get RTF file title
#'
#' Get the first text in the RTF file, remove '|' and '*', and trim whitespace
#'
#' @param p path to the file (character vector)
#'
#' @export
#'
rtf_get_title <- function(p, strip_non_alphanumeric=F) {
  rtf_text = striprtf::read_rtf(p, verbose = F, n=100)
  # First non-blank line contains the title
  t = striprtf::read_rtf(p)[min(which(rtf_text!=""))] %>%
    stringr::str_replace_all('[\\|\\*]','') %>%
    stringr::str_trim()

  if(strip_non_alphanumeric) {
    t = t %>% stringr::str_replace_all('[^[:alnum:]:()]', ' ')
  }

  return(t)
}
