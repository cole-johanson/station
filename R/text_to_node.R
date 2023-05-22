#' Convert text to a Microsoft Word paragraph
#'
#' @param t A character vector
#' @param bold Logical. Whether to bold the font or not.
#'
#' @export
text_to_node <- function(t, style = "Normal", bold = F, r = NULL) {
  if(style != 'Normal') {
    if(is.null(r)) rlang::abort('r must not be NULL if style is not "Normal"')
    style_id = r$styles %>% filter(style_name == style) %>% pull(style_id)
  } else {
    style_id = style
  }

  sup_pos = stringr::str_locate_all(t,'<sup>.</sup>')[[1]]
  sup_values = stringr::str_match_all(t,'<sup>(?<suptext>.)</sup>')[[1]][,"suptext"]

  get_nonsup_parts <- function(t, sup_pos) {
    to_pull = data.frame(start = 1, end = NA_integer_) %>%
      dplyr::bind_rows(as.data.frame(sup_pos)) %>%
      dplyr::bind_rows(data.frame(start = NA_integer_, end = nchar(t))) %>%
      mutate(str_start = coalesce(end+1,1), str_end = coalesce(dplyr::lead(start)-1,nchar(t))) %>%
      filter(str_end > str_start)
    parts = stringr::str_sub(t, to_pull$str_start, to_pull$str_end)
    return(parts)
  }

  nonsup_parts = get_nonsup_parts(t, sup_pos)

  text_node = suppressWarnings(xml2::read_xml(str_glue(
    wp_ns_yes,
    '<w:pPr><w:pStyle w:val="{style_id}"/></w:pPr></w:p>'
  )))

  superscript <- function(sup) {
    suppressWarnings(xml2::read_xml(str_glue(
      '<w:r><w:rPr><w:vertAlign w:val="superscript"/></w:rPr><w:t>{sup}</w:t></w:r>'
    ), ns = wp_ns_yes))
  }

  nonsuperscript <- function(nonsup) {
    nonsup_node = suppressWarnings(xml2::read_xml(str_c(
      '<w:r><w:t>',
      # Handle line splits
      str_c('<w:t xml:space="preserve">', stringr::str_split_1(text_escape_chars(nonsup), '\n'), '</w:t>', collapse = '<w:br/>'),
      '</w:t></w:r>'
    ), ns = wp_ns_yes))
  }

  # If the first pos is at the beginning of the string, insert first
  # Then alternate sups and nonsups.
  if(coalesce(dplyr::first(sup_pos)[,'start'],0) == 1) {
    for(i in 1:length(sup_values)) {
      text_node %>% xml2::xml_add_child(superscript(sup_values[i]))
      if(i <= length(nonsup_parts)) {
        text_node %>% xml2::xml_add_child(nonsuperscript(nonsup_parts[i]))
      }
    }
  } else {
    for(i in 1:length(nonsup_parts)) {
      text_node %>% xml2::xml_add_child(nonsuperscript(nonsup_parts[i]))
      if(i <= length(sup_values)) {
        text_node %>% xml2::xml_add_child(nonsuperscript(sup_values[i]))
      }
    }
  }

  return(text_node)
}

#' From officer::R/utils.R. Escape special characters.
#'
#' @export
text_escape_chars <- local({
  .htmlSpecials <- list(
    `&` = '&amp;',
    `<` = '&lt;',
    `>` = '&gt;'
  )
  .htmlSpecialsPattern <- paste(names(.htmlSpecials), collapse='|')
  .htmlSpecialsAttrib <- c(
    .htmlSpecials,
    `'` = '&#39;',
    `"` = '&quot;',
    `\r` = '&#13;',
    `\n` = '&#10;'
  )
  .htmlSpecialsPatternAttrib <- paste(names(.htmlSpecialsAttrib), collapse='|')
  function(text, attribute=FALSE) {
    pattern <- if(attribute)
      .htmlSpecialsPatternAttrib
    else
      .htmlSpecialsPattern
    text <- enc2utf8(as.character(text))
    # Short circuit in the common case that there's nothing to escape
    if (!any(grepl(pattern, text, useBytes = TRUE)))
      return(text)
    specials <- if(attribute)
      .htmlSpecialsAttrib
    else
      .htmlSpecials
    for (chr in names(specials)) {
      text <- gsub(chr, specials[[chr]], text, fixed = TRUE, useBytes = TRUE)
    }
    Encoding(text) <- "UTF-8"
    return(text)
  }
})

#' @export
# From officer::R/ooxml.R: wp_ns_yes is a w:p namespace
wp_ns_yes = "<w:p xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\" xmlns:wp=\"http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\" xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\">"
