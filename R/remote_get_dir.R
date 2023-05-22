#' Get remote directory
#'
#' Get the remote (V:/) directory of interest
#'
#' @param compound The compound of the study (directory in V:/). If NULL, infer from config.
#' @param study The study (directory in V:/[directory]). If NULL, infer from config.
#' @param task The study (directory in V:/[directory]). If NULL, infer from config.
#'
#' @export
#'
remote_get_dir <- function(compound = NULL, study = NULL, task = NULL) {
  if(is.null(compound)) {
    compound = get_golem_config('compound')
  }
  if(is.null(study)) {
    study = get_golem_config('study')
  }
  if(is.null(task)) {
    task = get_golem_config('task')
  }
  if(is.null(study) | is.null(compound) | is.null(task)) {
    rlang::abort("Missing golem config compound, study, or task")
  }

  return(str_glue('V:/{get_golem_config("compound")}/{get_golem_config("study")}/{get_golem_config("task")}/sp/outputs'))
}
