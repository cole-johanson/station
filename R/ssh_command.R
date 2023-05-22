#' SSH command
#'
#' Used in conjunction with the `config` package. Builds and calls an ssh call to run a command.
#'
#' @param command A string
#'
#' @export
ssh_command <- function(command) {
  remote_pem = config::get("remote_pem")
  remote_user = config::get("remote_user")
  remote_server = config::get("remote_server")
  ssh = str_glue('ssh -i "{remote_pem}" {remote_user}@{remote_server}')
  system(str_glue('{ssh} {command}'))
}
