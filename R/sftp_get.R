#' SFTP get a remote directory in a local directory
#'
#' Works in conjunction with the `config` package
#'
#' @param remote_directory The remote path to transfer the data to
#' @param local_data_directory A local directory containing data to transfer to a remote server
#'
#' @export
sftp_get <- function(remote_directory, local_data_directory) {
  remote_pem = config::get("remote_pem")
  remote_user = config::get("remote_user")
  remote_server = config::get("remote_server")

  batch_file_name = "sftp_batch.sh"
  batch_file = file(batch_file_name)
  get_command = str_glue('get -r {remote_directory} {local_data_directory}')
  writeLines(get_command, batch_file)
  close(batch_file)

  sftp = str_glue('sftp -b {batch_file_name} -i "{remote_pem}" {remote_user}@{remote_server}')
  system(str_glue("{sftp}"))
  unlink(batch_file_name)
}
