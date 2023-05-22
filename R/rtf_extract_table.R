#' Extract a table (data frame) from an RTF file
#'
#' @param file_path The path to the RTF file
#'
#' @export
rtf_extract_tables <- function(file_path, format_cells = T) {
  dfs = striprtf::read_rtf(file_path) %>%
    stringr::str_subset(pattern = '^\\*\\|.*') %>%
    tibble::as_tibble() %>%
    filter(
      stringr::str_trim(stringr::str_replace_all(value, '[\\|\\*]', '')) != ''
    ) %>%
    separate_and_title()

  if(format_cells) {
    dfs = purrr::map(dfs, table_format_cells)
  }

  if(ncol(dfs[[1]]) == 1 & nrow(dfs[[1]]) > 1) {
    rlang::warn(str_glue("Unable to process table {file_path}. File is not in tabular format."))
    # Find the first grepl('\\*\\|_+\\|') row. That starts the table.
    # Skip lines (should be 1) until the next that is at least 35 chars long
    # Define header_space as the number of spaces after '*| ' in the next row.
    # Define the columns as any set of two or more spaces in this first header row. Essentially,
    #   split by '  ', except we want to define each subsequent header_space as the halfway point
    #   between the non-space characters from where the double-space occurs to where the next
    #   double-space occurs  (74, 91, 117)
    # For each of the header rows, split the columns by the header_space  (74, round(mean(91,81)) = 86, round(mean(c(117,110))) = 114)
    # Collapse the headers str_c(str_trim(str_sub(74,86)) , ... , sep="\\n")
    # Find the next grepl('\\*\\|_+\\|') row. That ends the header.
    # Skip any lines like '\\*\\| [ ]+\\|' (just spaces)
    # Split all following rows by the same rules as the headers, expect make them new columns (do not collapse together)
    # Continue until another grepl('\\*\\|_+\\|')
    # Repeat until EOF
  }

  return(dfs)
}

#' Separate and title a data frame with character vector `value`
#'
#' Sets the names of the data frame and separates the `value` column into columns. Assumes the titles are
#' in the first row
#'
#' @param df A data frame
#' @param column_sep Regex to split the data frame by
#'
#' @export
separate_and_title <- function(df, column_sep = ' \\| ') {

  get_titles <- function(d) {
    d[1,] %>%
      stringr::str_split(pattern = ' \\| ') %>%
      unlist %>%
      v_rtf_format() %>%
      v_replace_empty_with_tmps() # rename '' with 'tmp1', 'tmp2', etc.
  }

  create_table <- function(d, column_sep = ' \\| ') {
    titles = get_titles(d)

    # Expect '*|', followed by some content (.*) and a column_sep (" | ") n-1 (because of the lagging empty
    # space) times
    expected_regex = str_c('\\*\\| ', str_c(rep(str_c('.*', column_sep), length(titles)-1), collapse=""))

    df_done = d[-1,] %>%
      # Filter to rows with some text.. previously grepl(expected_regex, value), but did not work for 601 14.2.5.2
      filter( !is.na(str_extract(value,'[A-Za-z0-9]+')) ) %>%
      # Need some logic here to detect when there is a differing number of columns, and split into a new
      tidyr::separate(value, into = titles, sep = column_sep) %>%
      dplyr::mutate_all(v_rtf_format) %>%
      filter_at( 1, all_vars(. != titles[1]) ) %>%
      # filter_at( 2:sum(!grepl("^tmp",titles)), all_vars(. != "") ) %>%
      select(-matches("^tmp[0-9]?")) %>%
      dplyr::mutate_if(is.character, ~if_else(is.na(.), "", .))
    return(df_done)
  }

  # n_cols = purrr::map_int(df$value, ~nrow(stringr::str_locate_all(., '\\|')[[1]]) -1)
  # new_table_starts = which(n_cols != dplyr::lag(n_cols))
  first_col_header = get_titles(df)[1]
  header_rows = which(grepl(str_glue('^\\*\\|{escape_regex(strip_whitespace(first_col_header))}\\|'),strip_whitespace(df$value)))
  headers = df$value[header_rows]
  new_header_rows = c(1,header_rows[which(headers != dplyr::lag(headers))])
  tables = list()

  for(i in 1:length(new_header_rows)) {
    start = new_header_rows[i]
    end = coalesce(new_header_rows[i+1]-1, nrow(df))
    tables = append(tables, list(create_table(df[start:end,])))
  }

  return(tables)
}

#' @export
strip_whitespace <- function(s) {
  str_replace_all(s, '\\s', '')
}

#' Replace empty with tmp
#'
#' @param x A vector
#'
#' @return A vector
#'
#' @export
v_replace_empty_with_tmps <- function(x) {
  for(i in 1:length(x)) {
    if(x[i] == '') x[i] = str_c('tmp',i)
  }
  return(x)
}

#' Separate and title a data frame with character vector `value`
#'
#' Sets the names of the data frame and separates the `value` column into columns. Assumes the titles are
#' in the first row
#'
#' @param s A vector of strings
#'
#' @export
v_rtf_format <- function(s) {
  stringr::str_replace_all(s, '[\\|\\*]', '')
}

