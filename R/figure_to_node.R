#' @export
figure_to_node <- function(fig_path) {
  # officer ooxml.R lines 1:38
  runs_to_p_wml <- function(..., add_ns = FALSE, style_id = NULL){
    wp_ns_no <- "<w:p>"
    wp_ns_yes <- "<w:p xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\" xmlns:wp=\"http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\" xmlns:w14=\"http://schemas.microsoft.com/office/word/2010/wordml\">"

    runs <- list(...)
    run_str <- lapply(runs, officer::to_wml, add_ns = FALSE)
    run_str$collapse <- ""
    run_str <- do.call(paste0, run_str)
    open_tag <- wp_ns_no
    if (add_ns) {
      open_tag <- wp_ns_yes
    }

    if( !is.null(style_id) )
      ppr <- paste0("<w:pPr><w:pStyle w:val=\"", style_id, "\"/></w:pPr>")
    else ppr <- "<w:pPr/>"
    out <- paste0(open_tag, ppr, run_str, "</w:p>")
    out
  }

  fig_dim = figure_dimensions(fig_path)
  # 6" is the width of the 8x11" with 1" margins
  if(fig_dim['width'] > 6) {
    fig_dim = fig_dim * (6 / fig_dim['width'])
  }

  value = officer::external_img(fig_path, width = fig_dim['width'], height = fig_dim['height'])
  xml_elt <- runs_to_p_wml(value, add_ns = TRUE)
  return(as_xml_document(xml_elt))
}

