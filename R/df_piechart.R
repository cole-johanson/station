#' Quick pie chart
#'
#' @param .data A data frame
#' @param pie_field The (unquoted) field in .data to create pie chart from
#' @param plot_title A character vector
#' @param legend_title A character vector
#' @param palette A palette of colors
#'
#' @export
df_piechart <- function(.data, pie_field, plot_title = '', legend_title = '', palette = station_palette) {
  pie_field = rlang::enquo(pie_field)

  .data = .data %>%
    mutate(
      !!pie_field := if_else(!!pie_field == '',' ',!!pie_field)
    ) %>%
    group_by(!!pie_field) %>%
    summarise(pie_n = n_distinct(SUBJID)) %>%
    arrange(desc(pie_n)) %>%
    mutate(
      pie_f = add_sorting(!!pie_field, 'order')
    )

  plotly::plot_ly(
    data = .data,
    labels = ~pie_f,
    values = ~pie_n,
    marker = list(
      colors = palette,
      line = list(color = '#FFFFFF', width = 1)
    ),
    type = 'pie'
  ) %>%
    plotly_custom_layout() %>%
    plotly::layout(
      legend = list(
        orientation = if_else(nrow(.data) < 5, 'h', 'v'),
        title=list(text=str_c('<b>',legend_title,'</b>'))
      ),
      showlegend=length(unique(.data$pie_f)) <= 7,
      title = list(text = plot_title, x = 0)
    )
}
