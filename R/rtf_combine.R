#'
#' This is heavily inspired by https://www.lexjansen.com/pharmasug/2010/PO/PO05.pdf
#'
#'
#' @export
rtf_combine <- function(files, output_path) {
  files = c("t_14_1_1_1_disp_ol.rtf", "f_14_4_1_2_1_1_ef_plsma_mitt_ol.rtf", "l_16_2_1_10_1_cm_diab_hyp_ol.rtf") %>%
    str_prefix("inst/app/www/")

  out = character()
  for(i in 1:length(files)) {
    f = readLines(files[i])

    # For all but the last, replace the last "}" with a "/sect"
    if(i < length(files)) {
      f = f %>%
        remove_last() %>% # Remove the '}' at the end of each file
        append('\\pard')
    } else {
      f = f %>%
        remove_last() %>% # Remove the '}' at the end of each file
        append('\\pard\\par}')
    }

    # For all, remove the extra headers and footers, and bookmarks
     f = f %>%
       remove_bookmarks() %>%
       stringr::str_replace_all('\\\\sect\\\\sectd','{\\fs2\\par\\page\\par}')

    # For all but the first, remove the document header info
    if(i > 1) {
      # lexjansen: It is not possible to concatenate two of these files together unless the file header
      # ({\rtf1\ansi ... {\info }) is removed from one and the closing brace is removed from the other.
      f = f %>%
        remove_first() %>% # Remove the {\rtf1\ansi at the beginning of all but the first file
        remove_rtf_tag('fonttbl') %>%
        remove_rtf_tag('colortbl') %>%
        remove_rtf_tag('stylesheet') %>%
        remove_rtf_tag('*\\cs10') %>%
        remove_rtf_tag('info') %>%
        remove_rtf_tag('header') %>%
        remove_rtf_tag('footer')
      # Add a page break... this probably isn't necessary if we wrap each file in /sect
      # first_section_line = dplyr::first(which(f %>% stringr::str_detect('\\\\sectd')))
      # f[first_section_line] = stringr::str_replace_all(f[first_section_line], '\\\\sectd', '\\\\sectd\\\\sect\\\\pagebb')
    } else {
      # For only the first, remove extra headers/footers
      f = f %>% remove_rtf_tag('header', keep_first=T) %>%
        remove_rtf_tag('footer', keep_first=T)
    }
    out = out %>% append(f)
  }
  file.remove(output_path)
  writeLines(out, output_path)
}

#' @export
remove_bookmarks <- function(f) {
  bookmark_regex = '\\{\\\\\\*\\\\bkmkstart [A-Za-z0-9]+\\}\\{\\\\\\*\\\\bkmkend [A-Za-z0-9]+\\}'
  bookmark_line = which(f %>% stringr::str_detect(bookmark_regex))
  if(length(bookmark_line) > 0) {
    f[bookmark_line] = stringr::str_replace_all(f[bookmark_line], bookmark_regex, '')
  }
  return(f)
}

#' @export
remove_rtf_tag <- function(f, tag = 'header', keep_first = F) {
  tag_regex = paste0('\\{\\\\',tag,'($|[^A-Za-z0-9])')
  tag_locs = which(f %>% stringr::str_detect(tag_regex))
  if(length(tag_locs) == 0) return(f)

  # Loop through backwards so we maintain tag_locs integrity as we delete lines
  for(i in length(tag_locs):1) {
    if(i==1 & keep_first) {
      tag_locs = tag_locs[-1]; next
    }

    tag_loc = tag_locs[i]
    tag_line = f[tag_loc]
    tag_loc_in_line = str_locate(tag_line, tag_regex)[1]
    tag_line_left = if_else(tag_loc_in_line==1,"",stringr::str_sub(tag_line, 1, tag_loc_in_line))
    tag_line_right = stringr::str_sub(tag_line, tag_loc_in_line, nchar(tag_line))

    opening_brackets <- function(s) {length(stringr::str_match_all(s,'\\{')[[1]])}
    close_brackets <- function(s) {length(stringr::str_match_all(s,'\\}')[[1]])}
    count_net_brackets <- function(s) {opening_brackets(s) - close_brackets(s)}

    net_brackets = opening_brackets(tag_line_right) - close_brackets(tag_line_right)
    j = 0 # j is the number of lines after the index (tag_loc) that the ending bracket occurs

    # New line brackets is the net number of brackets in each new line. We initialize with the first line
    new_line_brackets = count_net_brackets(f[tag_loc+j])
    while(net_brackets > 0) {
      j = j + 1
      new_line_brackets = count_net_brackets(f[tag_loc+j])
      net_brackets = net_brackets + new_line_brackets
    }

    # Need some logic to handle the case where j = 0, i.e. the bracket is closed on the same line it started.
    # Find the loc of the opening bracket: tag_loc_in_line
    # Count the number of  from count_net_brackets(stringr::str_sub(tag_line, tag_loc_in_line + 1, nchar(tag_line)))
    # Which closing bracket matches the opening bracket?
    # str_locate_nth(tag_line, '\\}', )

    str_locate_nth <- function(s, pattern, n) {
      m = stringr::str_locate_all(s,pattern)[[1]]
      as.integer(m[n,2])
    }
    closing_bracket_pos = str_locate_nth(f[tag_loc+j], '\\}',abs(new_line_brackets-net_brackets))
    f[tag_loc+j] = stringr::str_sub(f[tag_loc+j], closing_bracket_pos, length(f[tag_loc+j]))
    f = f[-(tag_loc:(tag_loc+j))]
  }
  return(f)
}

#' @export
remove_init_text <- function(f) {
  title_regex = '\\{\\\\info\\{\\\\title '
  title_line = which(f %>% stringr::str_detect(title_regex))
  f[(title_line+2):length(f)]
}

#' @export
remove_last <- function(x) {
  x[-length(x)]
}

#' @export
remove_first <- function(x) {
  x[-1]
}
