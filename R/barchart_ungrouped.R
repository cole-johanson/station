#' Bar chart
#'
#' Create a bar chart, either grouped by a `grouping_field`, or ungrouped.
#'
#' @param .data A data frame
#' @param bar_field The column to use as the bar_chart basis
#' @param xaxis_title A character vector
#' @param yaxis_title A character vector
#' @param bar_color A character vector of a hex color, or similarly handled color in Plotly.
#' @param height A real number to define the height of the plot (in pixels)
#' @param legend_title A character vector
#' @param plot_title A character vector
#' @param sort_method See `add_sorting`
#' @param grouping_mode Either "stack" or "group"
#' @param grouping_field The column in `.data` to group the bar chart by
#'
#' @export
barchart_ungrouped <- function(.data, bar_field, xaxis_title='', yaxis_title='', bar_color = '#26547C', height = NA_real_, legend_title='', plot_title='', sort_method = c('desc','alphabetical','numeric')) {
  sort_method = rlang::arg_match(sort_method)

  if(nrow(.data) == 0) return(NULL)
  bar_field = rlang::enquo(bar_field)

  d = .data %>%
    group_by(!!bar_field) %>%
    filter(coalesce(!!bar_field,"") != "") %>%
    summarise(
      n_res = n_distinct(SUBJID),
      subjects = str_c(unique(SUBJID), collapse = '<br>')
    )

  if(sort_method == 'desc') {
    d = d %>%
      arrange(n_res) %>%
      mutate(field_sorted = add_sorting(!!bar_field,"order"))
  } else if(sort_method == 'alphabetical') {
    d = d %>%
      arrange(n_res) %>%
      mutate(field_sorted = add_sorting(!!bar_field,"reverse_alphabetical"))
  } else {
    d = d %>%
      arrange(n_res) %>%
      mutate(field_sorted = add_sorting(!!bar_field,"reverse_numeric"))
  }

  p = plotly::plot_ly(
    d,
    #x = rlang::new_formula(NULL, quote(expr(!!bar_field))),
    x = ~n_res,
    y = ~field_sorted,
    type='bar',
    text = ~subjects,
    textposition = "none",
    hovertemplate = '%{y}<br>%{text}<extra></extra>', # <extra></extra> removes "trace0"
    marker = list(color = bar_color),
    height = coalesce(height,pmax(nrow(d) * 21, 400))
  ) %>%
    plotly::add_text(
      x = ~n_res + max(n_res)*0.01,
      y=~field_sorted,
      text = ~n_res,
      textposition = "right",
      showlegend = F,
      marker = NULL
    ) %>%
  plotly::layout(
    #xaxis=list(tickformat=',d'),
    xaxis=list(showticklabels = F),
    yaxis=list(tickfont = list(size = 15)),
    legend=list(title=legend_title, y = 1.05),
    bargap=0.15,
    title = plot_title
  ) %>%
  plotly_custom_layout(xaxis_title=xaxis_title, yaxis_title=yaxis_title)
  return(p)
}
