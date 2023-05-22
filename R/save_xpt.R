#' Save XPT
#'
#' Save a file as an XPT file
#'
#' @param x a data frame to save
#' @param ... strings to be pasted together to form a path (via `fs::path(...)`)
#'
#' @export
save_xpt <- function(x, ...) {
  path_ = fs::path(...)
  path_dir = str_replace(path_,'/[^/]*$', '')
  if(!dir.exists(path_dir)) {
    dir.create(path_dir)
  }
  if(!str_detect(path_,'\\.xpt$')) path_ = str_c(path_,'.xpt')
  haven::write_xpt(x, path_, version = 8)
}
