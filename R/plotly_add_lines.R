#' Add lines
#'
#' Add lines to a plot. Unfortunately, this can only be called once per plotly::plot_ly(), so gather the
#' line requirements prior to call. Most of the parameters are extensible for multiple lines.
#'
#' @param p a plotly object
#' @param ref_val The locations om the `refaxis` to draw the line (an x-axis or y-axis value)
#' @param color The color of the line
#' @param text The text to display for the line
#' @param dash_type Character vector passed to plotly `shapes$line$dash`
#' @param refaxis Use "x" or "y", or if there are multiple y-axes, the trace yaxis specified
#' @param linetype 'horizontal' or 'vertical'
#'
#' @export
plotly_add_lines <- function(p, ref_val = c(-.8,-.3,0), color = '#26547C', text = "ULN", dash_type="dot", refaxis="y", linetype = 'horizontal') {
  shapes = list()
  annotations = list()

  if(length(color)==1) {
    color = rep(color,length(ref_val))
  }
  if(length(text)==1) {
    text = rep(text,length(ref_val))
  }
  if(length(refaxis)==1) {
    refaxis = rep(refaxis,length(ref_val))
  }
  if(length(linetype)==1) {
    linetype = rep(linetype,length(ref_val))
  }

  if(!all(linetype %in% c('horizontal','vertical'))) {
    rlang::abort("linetype must be one of c('horiztontal','vertical')")
  }

  for(i in 1:length(ref_val)) {
    if(linetype[i] == 'horizontal') {
      shapes[[i]] = list(
        type = "line",
        y0 = ref_val[i],
        y1 = ref_val[i],
        yref = refaxis[i],
        xref = "paper",
        x0 = 0,
        x1 = 1,
        line = list(color = color[i], dash=dash_type)
      )
      annotations[[i]] = list(
        x = 1,
        y = ref_val[i],
        xanchor = "right",
        xref = "paper",
        font = list(color = color[i], family="Arial"),
        text = text[i],
        showarrow = FALSE,
        xref = "x",
        yref = refaxis[i],
        textangle = 0,
        yshift = 10
      )
    } else {
      #linetype == 'Vertical'
      shapes[[i]] = list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        xref=refaxis[i],
        x0 = ref_val[i],
        x1 = ref_val[i],
        line = list(color = color[i], dash=dash_type)
      )
      annotations[[i]] = list(
        x = ref_val[i],
        y = 0,
        yanchor = "bottom",
        font = list(color = color[i], family="Arial"),
        text = text[i],
        showarrow = FALSE,
        xref = "x",
        yref = "paper",
        textangle = 90,
        xshift = -10
      )
    }
  }

  p %>% plotly::layout(
    shapes = shapes,
    annotations = annotations
  )
}

