#' @describeIn barchart_ungrouped Grouped bar chart
#' @export
barchart_grouped <- function(.data, bar_field, grouping_field = NULL, xaxis_title='', yaxis_title='', grouping_mode = c("stack","group"), legend_title='', plot_title='') {
  if(nrow(.data) == 0) return(NULL)
  grouping_mode = rlang::arg_match(grouping_mode)
  bar_field = rlang::enquo(bar_field)
  grouping_field = rlang::enquo(grouping_field)

  d = .data %>%
    filter(coalesce(!!bar_field,'') != '') %>%
    filter(coalesce(!!grouping_field,'') != '') %>%
    group_by(!!bar_field, !!grouping_field) %>%
    summarise(
      n = n_distinct(SUBJID),
      pts = str_c(unique(SUBJID),collapse='<br>'),
      .groups='drop'
    ) %>%
    group_by(!!bar_field) %>%
    mutate(sum_all = sum(n)) %>%
    arrange(sum_all) %>%
    ungroup %>%
    mutate(
      barred_field = add_sorting(!!bar_field, "order"),
      group_field = add_sorting(!!grouping_field, "reverse_numeric"),
      text = str_c(!!bar_field,!!grouping_field,pts,sep='<br>')
    ) # Reverse

  n_groups = length(unique(d$group_field))

  text_data = d %>%
    distinct(barred_field, sum_all)

  plotly::plot_ly(
    d,
    height = pmax(length(unique(d$barred_field)) * 23, 400)
  ) %>%
    plotly::add_bars(
      x = ~n,
      y=~barred_field,
      color=~group_field,
      colors = cohort_palette,
      text=~text,
      textposition = "none",
      texttemplate = '%{text}',
      hoverinfo = 'text'
    ) %>%
    plotly::add_text(
      data = text_data,
      x = ~sum_all + max(sum_all)*0.01,
      y=~barred_field,
      text = ~sum_all,
      textposition = "right",
      showlegend = F
    ) %>%
    plotly::layout(
      barmode = grouping_mode,
      xaxis=list(showticklabels = F),
      yaxis = list(
        tickfont = list(size = 15)
      ),
      legend=list(title=legend_title, y = 1.05),
      title = plot_title
    ) %>%
    plotly_custom_layout(
      xaxis_title = xaxis_title,
      yaxis_title = yaxis_title,
      legend_orientation = 'v'
    )
}
