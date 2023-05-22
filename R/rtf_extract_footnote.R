#' Extract the footnote from an RTF file
#'
#' See
#'
#' @param file_path Path to an RTF file
#'
#' @export
rtf_extract_footnote <- function(file_path) {
  striprtf::read_rtf_footer(file_path)
    # str_replace_all('([^^])[Nn]ote:', '\\1\nNote:') # Do not need with \n handling

  # striprtf::read_rtf(file_path) %>%
  #   stringr::str_subset(pattern = '^\\*\\|.*') %>%
  #   pull_last_line() %>%
  #   str_replace('^\\*\\| ', '') %>%
  #   str_replace(' \\| $', '') %>%
  #   str_replace('Page [0-9]+ of [0-9]+', '')
}

#' Pull last element of a vector
#'
#' @param v A vector
#'
#' @export
pull_last_line <- function(v) {
  v[length(v)]
}
