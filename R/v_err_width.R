#' Get Error Width (using T-test)
#'
#' @param x A numeric vector
#'
#' @export
v_err_width <- function(x, ci = 0.95, error_calc = c("Normal","Percent")) {
  error_calc = rlang::arg_match(error_calc)
  # This is different because geom_errorbar accepts actual values but plotly accepts width
  if(length(x)<2) {return(0)} # TODO: test this to see if it doesn't create an error bar

  if(error_calc == "Normal") {
    if(n_unique(x) == 1) return(0)
    t_test = t.test(x)
    t_stat = qt(1-(1-ci)/2,t_test$parameter) # Get the t distribution statistic
    error_width = t_stat*t_test$stderr
  } else {
    # error_calc == "Percent"
    # TODO: add percent calculations of error width
  }
  return(error_width)
  # t_test$estimate-t_test$conf.int[1] This should be identical... We can add a test for that at some point.
}
