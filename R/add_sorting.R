#' Add sorting
#'
#' Sort based on some criteria
#'
#' @param x A categorical vector
#' @param method One of:
#' \itemize{
#'   \item{"count_desc"} {Descending count},
#'   \item{"order"} {Order in which the element is first seen}
#'   \item{"reverse_alphabetical"} {Reverse alphabetical order}
#'   \item{"numeric"} {Sort by [0-9] values found in text}
#' }
#'
#' @export
add_sorting <- function(x, method="count_desc") {
  x = as.character(x)
  if(method == "count_desc") {
    levels = tibble(x=x) %>%
      group_by(x) %>%
      summarise(n=dplyr::n()) %>%
      arrange(desc(n)) %>%
      pull(x)
  } else if(method == "order") {
    levels = unique(x)
  } else if(method == "reverse_order") {
    levels = rev(unique(x))
  } else if(method == "reverse_alphabetical") {
    levels = sort(unique(x),decreasing = TRUE)
  } else if(method == "alphabetical") {
    levels = sort(unique(x),decreasing = FALSE)
  } else if(method == "numeric") {
    levels = tibble::tibble(x=unique(x)) %>%
      mutate(n_ = as.numeric(str_extract(x,'\\d+'))) %>%
      arrange(n_) %>% pull(x)
  } else if(method == "reverse_numeric") {
    levels = tibble::tibble(x=unique(x)) %>%
      mutate(n_ = as.numeric(str_extract(x,'\\d+'))) %>%
      arrange(desc(n_)) %>% pull(x)
  } else {
    rlang::abort(stringr::str_glue('Unsupported method: {method}'))
  }

  factor(x,levels=levels)
}
