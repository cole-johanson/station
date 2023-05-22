#' WebPro Plotly Layout
#'
#' A custom to layout to apply to Plotly objects
#'
#' @param .p a Plotly object
#' @param xaxis_title a character vector
#' @param yaxis_title a character vector
#'
#' @export
plotly_custom_layout <- function(.p, xaxis_title = '', yaxis_title = '', showgrid=F, showzeroline=F, legend_orientation='h') {
  .p %>%
    plotly::layout(
      yaxis = list(title = yaxis_title, showgrid=showgrid, zeroline=showzeroline),
      xaxis = list(title = xaxis_title, showgrid=showgrid, zeroline=showzeroline),
      paper_bgcolor = invisible,
      plot_bgcolor = invisible,
      legend = list(orientation=legend_orientation)
    )
}

#' @export
invisible='rgba(0,0,0,0)'
