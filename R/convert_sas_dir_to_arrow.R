#' Convert SAS data in directory to arrow
#'
#' Converts all SAS files (.sas7bdat) in a diretory to feather files (.arrow) in the same directory
#'
#' @param ... strings to be joined together into a path. (Can contain slashes but not needed)
#'
#' @return NULL
#'
#' @export
convert_sas_dir_to_arrow <- function(...) {
  path_ = fs::path(...)
  files = list.files(path_)
  
  sas_files = files[str_which(files,'\\.sas7bdat$')]
  purrr::map(sas_files,~convert_sas_file_to_arrow(fs::path(path_,.)))
  return(NULL)
}