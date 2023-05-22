#' Save SAS
#'
#' Save a file as a SAS file
#'
#' @param x a data frame to save
#' @param ... strings to be pasted together to form a path (via `fs::path(...)`)
#'
#' @export
save_sas <- function(x, ...) {
  path_ = fs::path(...)
  path_dir = str_replace(path_,'/[^/]*$', '')
  if(!dir.exists(path_dir)) {
    dir.create(path_dir)
  }
  if(!str_detect(path_,'\\.sas7bdat$')) path_ = str_c(path_,'.sas7bdat')
  haven::write_sas(x, path_)
}