#' Kableize a data frame
#'
#' This function takes a data frame with a first column of row headers and the second column as a cohort of
#' patients. The first row, "n_pts", represents the number of patients in each cohort.
#'
#' The function builds a kable object, appending "(N={n_pts})". It optionally adds indenting based on an
#' `indent` column.
#'
#' @param t A data frame
#' @param var_col_name The desired output name of the first column
#' @param val_col_name The desired output name of the second column (which will be appended with "(N={n_pts})")
#'
#' @export
df_kableize <- function(t, var_col_name, val_col_name = 'All subjects') {
  n_pts = t %>% slice(1) %>% pull(val)
  k = t %>%
    slice(-1)
  indent_rows = which(k$indent)
  k = t %>%
    select(-indent) %>%
    slice(-1) %>%
    mutate(val = as_table1_percent(val, n_pts)) %>%
    rlang::set_names(c(var_col_name,str_c(val_col_name, str_glue('(N={n_pts})'), sep='<br>'))) %>%
    mutate_if(is.character, ~coalesce(.,""))

  k %>%
    knitr::kable("html", escape=F) %>%
    kableExtra::add_indent(indent_rows) %>%
    kableExtra::kable_styling()
}
