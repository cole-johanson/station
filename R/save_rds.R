#' Save RDS
#'
#' Save a file as am RDS file
#'
#' @param x a data frame to save
#' @param ... strings to be pasted together to form a path (via `fs::path(...)`)
#'
#' @export
save_rds <- function(x, ...) {
  path_ = fs::path(...)
  path_dir = str_replace(path_,'/[^/]*$', '')
  if(!dir.exists(path_dir)) {
    dir.create(path_dir)
  }
  if(!str_detect(path_,'\\.rds$')) path_ = str_c(path_,'.rds')
  readr::write_rds(x, path_)
}