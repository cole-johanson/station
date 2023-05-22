#' Convert SAS file to arrow
#'
#' Converts a SAS file (.sas7bdat) to a feather file (.arrow) in the same directory
#'
#' @param ... strings to be joined together into a path. (Can contain slashes but not needed)
#'
#' @return NULL
#'
#' @export
convert_sas_file_to_arrow <- function(...) {
  path_ = fs::path(...)
  target_path = str_replace(path_,'\\.sas7bdat$','\\.arrow')
  haven::read_sas(fs::path(...)) %>% arrow::write_feather(target_path)
  return(NULL)
}