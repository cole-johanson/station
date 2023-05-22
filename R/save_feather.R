#' Save feather
#'
#' Save a file as a feather file
#'
#' @param x a data frame to save
#' @param ... strings to be pasted together to form a path (via `fs::path(...)`)
#'
#' @export
save_feather <- function(x, ...) {
  path_ = fs::path(...)
  path_dir = str_replace(path_,'/[^/]*$', '')
  if(!dir.exists(path_dir)) {
    dir.create(path_dir)
  }
  if(!str_detect(path_,'\\.arrow$')) path_ = str_c(path_,'.arrow')
  arrow::write_feather(x, path_)
}