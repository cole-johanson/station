#' Remove text (docx)
#'
#' @param r an officer::rdocx object
#' @param location_regex The regex to search for (for inserting the text)
#'
#' @export
docx_remove_text <- function(r, location_regex) {
  r = r %>%
    officer::cursor_reach(location_regex) %>% # Set the cursor at text regex
    officer::body_remove()

  return(x)
}
