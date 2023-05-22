#' NavigatR file copy
#'
#' Wrapper around fs::file_copy to fs::dir_create() if necessary
#'
#' @param ... see \link[fs]{file_copy}
#'
#' @export
#'
file_copy_to <- function(...) {
  args = list(...)
  if(length(args) < 2) {
    rlang::abort('file_copy requires at least two arguments')
  }
  # Hack to get the new_path based on position or name
  new_path = NULL
  if(!is.null(args[['new_path']])) {
    new_path = args[['new_path']]
  } else {
    new_path = args[[2]]
  }

  purrr::map(new_path, create_directory_for_file) # TODO: Don't print anything

  do.call(fs::file_copy, args)
}

#' Create directories
#'
#' Creates local directories (if needed) based on file path(s)
#'
#' @param file a single string or a vector of strings representing file names
#'
#' @export
create_directory_for_file <- function(file) {
  dir_to_create = file_get_directory_path(file)
  if(!is.na(dir_to_create)) fs::dir_create(dir_to_create)
}

#' Get file name
#'
#' Get the file name from a file path
#'
#' @param p a character vector representing a file path
#'
#' @export
file_name <- function(p) {
  if_else(
    !is.na(str_locate_last(p,'/')),
    str_sub(p, start = str_locate_last(p,'/') + 1),
    p
  )
}

#' Get file directory
#'
#' Get the file directory given a full file path
#'
#' @param p a character vector representing a file path
#'
#' @export
file_get_directory_path <- function(p) {
  str_sub(p, end = str_locate_last(p,'/') - 1)
}
