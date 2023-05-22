#' @export
file_type <- function(file_wo_suffix) {
  type = case_when(
    grepl('^[Tt]',file_wo_suffix)~'Table',
    grepl('^[Ll]',file_wo_suffix)~'Listing',
    grepl('^[Ff]',file_wo_suffix)~'Figure',
    TRUE~'Not matched'
  )
  if(any(type == 'Not matched')) {
    rlang::warn(
      str_glue('{length(type[type == "Not matched"])} file(s) not matched and will be removed:\n',
        str_c('  ', file_wo_suffix[which(type == 'Not matched')], collapse = '\n' )
      )
    )
  }
  return(type)
}
