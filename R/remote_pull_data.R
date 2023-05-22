#' Pull remote data
#'
#' Pulls remote data as-is into `local_data_directory`. This function pulls the `files_to_pull` over into
#' a timestamped directory (with today's date).
#'
#' @param remote_files_of_interest A list of vectors of files. See Details.
#' @param remote_dir The study directory on the remote server.
#' @param local_data_directory The local directory in which to create a timestamped directory containing the data.
#' @param overwrite logical; should existing destination files be overwritten? (Passed to base::file.copy)
#' @param sub_dir The specific subdirectories of the remote path to search. For example, if the
#' `remote_path` is V:/compound/study and the `sub_dir` is '/sp/outputs', the paths of
#' V:/compound/study/.../sp/outputs/ will be searched.
#' @param overwrite Logical. If `TRUE`, overwrite existing files.
#'
#' @details If the directory/data exists, the function will not delete any files, but will replace any
#' existing data.
#'
#' Note that `remote_files_of_interest` should have the following structure:
#' \itemize{
#'   \item{Remote}{ `remote_dir`/`names(tasks_and_files)`/`remote_sub`/`values(tasks_and_files)` }
#'   \item{Local}{ ~/`local_data_directory`/`names(tasks_and_files)`/`values(tasks_and_files)` }
#' }
#'
#' @export
#'
remote_pull_data <- function(
    remote_files_of_interest,
    local_data_directory=get_golem_config('local_data_directory'),
    overwrite=T
  ) {
  local_files = fs::path(local_data_directory,file_get_name(remote_files_of_interest))
  purrr::map2(remote_files_of_interest, local_files, nv_file_copy, overwrite=overwrite)

  return(invisible())
}
