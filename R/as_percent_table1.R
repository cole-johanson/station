#' @export
as_percent_table1 <- function(n,d, round=2) {
  if_else(
    is.na(n) | is.na(d),
    NA_character_,
    as.character(str_glue('{n} ({as_percent(n/d, round=round)})'))
  )
}
