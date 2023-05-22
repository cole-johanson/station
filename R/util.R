#' Locate the last location of a character in a string
#'
#' See \link[stringr]{str_sub}
#'
#' @param string Input vector. Either a character vector, or something coercible to one.
#' @param pattern Pattern (Character) to look for.
#'
#' @export
#'
str_locate_last <- function(string, pattern) {
  locs = stringr::str_locate_all(string , pattern)
  get_last <- function(m) {
    if(nrow(m) == 0) return(NA_integer_)
    max(m[,2])
  }
  as.integer(purrr::map(locs, get_last))
}

navigatr_subheader <- function() {
  str_c(
    title_compound(get_golem_config('compound')),
    title_study(get_golem_config('study')),
    get_golem_config('task'),
    sep = ' / '
  )
}

#' Task title
#'
#' Logic per Raj 2023-04-03
#'
#' @param string A task name
#'
#' @export
title_task <- function(string) {
  case_when(
    grepl('csr|dsur',string)~toupper(string),
    TRUE~str_to_title(string)
  )
}

#' Compound title
#'
#' Logic per Raj 2023-04-03
#'
#' @param string A compound ID
#'
#' @export
title_compound <- function(string) {
  case_when(
    string == 'cort118335'~'Miricorilant (Cort118335)',
    string == 'cort125134'~'Relacorilant (Cort125134)',
    string == 'cort113176'~'Dazucorilant (Cort113176)', # last
    string == 'cort125281'~'Exicorilant (Cort125281)',
    TRUE~string
  )
}

#' Study title
#'
#' Logic per Raj 2023-04-03
#'
#' @param string A study number
#'
#' @export
title_study <- function(string) {
  string = as.character(string)
  case_when(
    string == '556'~'Rosella (556)',
    string == '455'~'Grace (455)',
    string == '456'~'Gradient (456)',
    string == '652'~'DAZALS (652)',
    TRUE~string
  )
}

#' Name/Title compounds, studies, and tasks in a data frame
#'
#' @param df A data frame with compound, study, and task columns
#'
#' @export
name_and_title_df <- function(df) {
  if(any(!c('compound', 'study', 'task') %in% colnames(df))) rlang::abort('df must contain compound, study, and task columns')

  df %>%
    mutate(
      compound = title_compound(compound),
      study = title_study(study),
      task = title_task(task)
    )
}

#' List the files in a directory
#'
#' This function automatically removes temporary files (i.e. those with '~' in the path)
#'
#' @param directory_path A string representing a directory (local or remote)
#' @param sub_dir An optional subdirectory. This is a separate parameter so we can loop over directories
#'               and access the same subdirectories (built for V:/.../sp/outputs/)
#' @param file_regex A string representing a regex to filter to
#'
#' @export
#'
list_files <- function(directory_path=getwd(), file_regex='.*', recursive=FALSE) {
  # Remove some tmp files ('~...rtf'); subset to '.rtf' files
  files = str_subset(
    str_subset(
      # subset to '.rtf' files
      list.files(directory_path, recursive=recursive),
      file_regex
    ),
    # Remove some tmp files ('~...rtf')
    '.*~.*', negate = TRUE
  )

  str_c(directory_path,files,sep='/')
}

#' NavigatR file copy
#'
#' Wrapper around fs::file_copy to fs::dir_create() if necessary
#'
#' @param ... see \link[fs]{file_copy}
#'
#' @export
#'
nv_file_copy <- function(...) {
  args = list(...)
  if(length(args) < 2) {
    rlang::abort('file_copy requires at least two arguments')
  }
  # Hack to get the new_path based on position or name
  new_path = NULL
  if(!is.null(args[['new_path']])) {
    new_path = args[['new_path']]
  } else {
    new_path = args[[2]]
  }

  file_create_directory(new_path)

  do.call(fs::file_copy, args)
}

#' Prefix string
#'
#' Used for piping: apply the following parameters prior to the first
#'
#' @param string a string
#' @param ... see \link[stringr]{str_c}
#' @param sep see \link[stringr]{str_c}
#'
#' @export
#'
str_prefix <- function(string, ..., sep= "") {
  stringr::str_c(..., string, sep=sep)
}

#' Prefix string
#'
#' Used for piping: apply the following parameters prior to the first
#'
#' @param string A string
#' @param pattern Pattern to remove. See \link[stringr]{str_replace}
#'
#' @export
#'
str_remove <- function(string, pattern) {
  stringr::str_replace(string, pattern, replacement = '')
}

#' SSH command
#'
#' @param command A string
#'
#' @export
ssh_command <- function(command) {
  remote_pem = get_golem_config("remote_pem")
  remote_user = get_golem_config("remote_user")
  remote_server = get_golem_config("remote_server")
  ssh = str_glue('ssh -i "{remote_pem}" {remote_user}@{remote_server}')
  system(str_glue('{ssh} {command}'))
}

#' SFTP put a local directory to a remote directory
#'
#' @param local_data_directory A local directory containing data to transfer to a remote server
#' @param remote_directory The remote path to transfer the data to
#'
#' @export
sftp_put <- function(local_data_directory, remote_directory) {
  remote_pem = get_golem_config("remote_pem")
  remote_user = get_golem_config("remote_user")
  remote_server = get_golem_config("remote_server")

  batch_file_name = "sftp_batch.sh"
  batch_file = file(batch_file_name)
  put_command = str_glue('put -r {local_data_directory} {remote_directory}')
  writeLines(put_command, batch_file)
  close(batch_file)

  sftp = str_glue('sftp -b {batch_file_name} -i "{remote_pem}" {remote_user}@{remote_server}')
  system(str_glue("{sftp}"))
  unlink(batch_file_name)
}

#' SFTP get a remote directory in a local directory
#'
#' @param remote_directory The remote path to transfer the data to
#' @param local_data_directory A local directory containing data to transfer to a remote server
#'
#' @export
sftp_get <- function(remote_directory, local_data_directory) {
  remote_pem = get_golem_config("remote_pem")
  remote_user = get_golem_config("remote_user")
  remote_server = get_golem_config("remote_server")

  batch_file_name = "sftp_batch.sh"
  batch_file = file(batch_file_name)
  get_command = str_glue('get -r {remote_directory} {local_data_directory}')
  writeLines(get_command, batch_file)
  close(batch_file)

  sftp = str_glue('sftp -b {batch_file_name} -i "{remote_pem}" {remote_user}@{remote_server}')
  system(str_glue("{sftp}"))
  unlink(batch_file_name)
}

#' Unbold if clicked
#'
#' Or, really, bold if clicked
#'
#' @param title A string
#' @param clicked A boolean
#'
#' @export
unbold_if_clicked <- function(title, clicked) {
  dplyr::if_else(
    clicked,
    HTML(str_glue("<span style=\"color:{wp_palette['light_gray_bg_border']}\">{title}</span>")),
    HTML(str_glue('<b>{title}</b>'))
  )
}

#' @export
remove_na <- function(x) {
  x[!is.na(x)]
}

#' @export
escape_regex <- function(string) {
  # Define regular expression special characters
  special_chars <- c("\\\\", "\\^", "\\$", "\\.", "\\|", "\\?", "\\*", "\\+", "\\(", "\\)", "\\[", "\\{")

  # Use gsub to replace each special character with its escaped version
  for (char in special_chars) {
    string <- gsub(char, paste0("\\", char), string)
  }

  return(string)
}
