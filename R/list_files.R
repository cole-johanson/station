#' List the files in a directory
#'
#' This function automatically removes temporary files (i.e. those with '~' in the path)
#'
#' @param directory A string representing a directory (local or remote)
#' @param sub_dir An optional subdirectory. This is a separate parameter so we can loop over directories
#'               and access the same subdirectories (built for V:/.../sp/outputs/)
#' @param file_regex A string representing a regex to filter to
#'
#' @export
#'
list_files <- function(directory=getwd(), file_regex='.*\\.rtf$', recursive=TRUE, subdirs_to_ignore = c('archive')) {
  # Remove some tmp files ('~...rtf'); subset to '.rtf' files
  files = list.files(directory, recursive=recursive) %>%
    str_subset(file_regex) %>%
    str_subset('.*~.*', negate = TRUE) %>% # Remove some tmp files ('~...rtf')
    str_subset(str_c(str_c(subdirs_to_ignore,'/'), collapse='|'), negate = T) # ignore_subdirs (recursively)

  return(files)
}
