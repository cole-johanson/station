#' Extract SAS table
#'
#' @param sas_path The path to the .sas7bdat file
#'
#' @export
sas_extract_table <- function(sas_path) {
  haven::read_sas(sas_path) %>%
    use_labels_as_headers()
}

#' Use labels as headers for a data frame
#'
#' Uses the labels as the column names for a data frame
#'
#' @param t A data frame
#'
#' @export
use_labels_as_headers <- function(t) {
  t %>% dplyr::rename(v_invert(sjlabelled::get_label(t)))
}
