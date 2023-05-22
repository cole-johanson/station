#' Read saved file
#'
#' Read a saved feather file
#'
#' @param ... strings to be pasted together to form a path (via `fs::path(...)`)
#'
#' @export
read_feather <- function(...) {
  path_ = fs::path(...)
  if(!str_detect(path_,'\\.arrow$')) path_ = str_c(path_,'.arrow')
  arrow::read_feather(path_)
}