#' Retitle RTF file
#'
#' RTF files contain a title that gets displayed at the top of the PDF file (when converted). This function
#' retitles the RTF file.
#'
#' @param file a single string or a vector of strings representing file names
#' @param new_title
#'
#' @export
rtf_reformat <- function(file, new_title = NULL) {
  f = readr::read_lines(file)

  ##########################################################################################################
  #### Replace the title with the new_title
  ##########################################################################################################
  if(!is.null(new_title)) {
    title_regex = '\\{\\\\info\\{\\\\title '
    file_line_number = which(stringr::str_detect(f, title_regex))
    if(!length(file_line_number) == 0) {
      file_line = f[file_line_number]
      end_of_title_regex = stringr::str_locate(file_line, title_regex)[,2]
      close_brackets_within_file = stringr::str_locate_all(file_line, '\\}')[[1]][,2]
      first_close_bracket_after_title = close_brackets_within_file[close_brackets_within_file>end_of_title_regex][1]
      new_file_line = stringr::str_c(
        stringr::str_sub(file_line, end = end_of_title_regex), #up to the title
        new_title,
        stringr::str_sub(file_line, start = first_close_bracket_after_title) #after the title
      )
      f[file_line_number] = new_file_line
    }
  }

  ##########################################################################################################
  #### Add a \pagebb after any \sect (to ensure LibreOffice treats the page as a new page)
  ##########################################################################################################
  sect_regex = '\\\\sect\\\\(?!pagebb)'
  detected_sections = which(stringr::str_detect(f, sect_regex))
  # Just using a str_replace_all blindly replaced special strings like "\xa0" with "�", so we specify..
  if(length(detected_sections) > 0) {
    f[detected_sections] = stringr::str_replace_all(f[detected_sections], sect_regex, '\\\\sect\\\\pagebb\\\\')
  }

  ##########################################################################################################
  #### Replace Page [0-9]+ of [0-9]+ with Page PAGE of NUMPAGES
  ##########################################################################################################
  page_regex = 'Page [0-9]+ of [0-9]+'
  detected_page_regex = which(stringr::str_detect(f, page_regex))
  replacement_regex = 'Page {\\\\field{\\*\\\\fldinst { PAGE }}}{ of }{\\\\field{\\*\\\\fldinst { NUMPAGES }}}' %>%
    escape_regex()
  # Just using a str_replace_all blindly replaced special strings like "\xa0" with "�", so we specify..
  if(length(detected_page_regex) > 0) {
    f[detected_page_regex] = stringr::str_replace_all(f[detected_page_regex], page_regex, replacement_regex)
  }

  writeLines(f,file)
  return(invisible())
}
