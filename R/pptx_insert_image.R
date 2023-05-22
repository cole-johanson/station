#' Insert a figure into a officer::rpptx object
#'
#' @param x An officer::rpptx object
#' @param fig_path Path to the figure to be inserted
#' @param location_regex The regex to search for within `x` where to insert the figure
#'
#' @export
pptx_insert_figure <- function(x, fig_path, location_regex) {
  fig_dim = figure_dimensions(fig_path)

  # Find which slide/section contains the location_regex
  slide_with_regex = NULL
  label_with_regex = NULL
  for(i in 1:length(x)) {
    slide_info = officer::slide_summary(x, i)
    which_section_has_regex = which(grepl(location_regex, slide_info$text))
    if(length(which_section_has_regex) > 0) {
      slide_with_regex = i
      label_with_regex = slide_info[which_section_has_regex, 'ph_label']
      break
    }
  }

  x = x %>%
    officer::on_slide(slide_with_regex) %>% # Set the cursor at text regex
    officer::ph_with(
      value = officer::external_img(fig_path, width = fig_dim['width'], height = fig_dim['height']),
      location = officer::ph_location_label(ph_label = label_with_regex)
    )

  return(x)
}
