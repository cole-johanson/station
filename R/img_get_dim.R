#' Get figure dimensions
#'
#' @param fig_path The relative path of the figure to insert
#'
#' @return a named real number vector
#'
#' @export
figure_dimensions <- function(fig_path = file.path( R.home("doc"), "html", "logo.jpg" )) {
  fig_suffix = tolower(file_get_suffix(fig_path))

  fig_dim = NULL
  if(fig_suffix == 'emf') {
    fig_dim = pixel_to_inch(fig_get_dim_emf(fig_path))
  } else if(fig_suffix %in% c('jpg', 'jpeg')) {
    fig_dim = pixel_to_inch(dim(
      jpeg::readJPEG(fig_path)
    ))[1:2]
    names(fig_dim) = c("height", "width")
  } else if(fig_suffix %in% c('png')) {
    fig_dim = pixel_to_inch(dim(
      png::readPNG(fig_path)
    ))[1:2]
    names(fig_dim) = c("height", "width")
  }

  return(fig_dim)
}

#' @describeIn figure_dimensions Convert pixels to inches
#'
#' @param pixel The pixel count, returned from a dim() call
#' @param ppi The pixels per inch to use. The usual pixel count of monitors is 94 and 120 pixels per inch
#' (PPI). One site said the most common is 96, so we use that as the default.
#'
#' @export
pixel_to_inch <- function(pixel, ppi = 96) {
  pixel / ppi
}

#' @describeIn figure_dimensions Get EMF file dimensions
#' @export
fig_get_dim_emf <- function(fig_path) {
  x = file(fig_path)

  r = readBin(fig_path, "raw", 24)

  width_raw = r[17:20]
  height_raw = r[21:24]

  return(c(height = raw_to_32bit_unsigned(height_raw), width = raw_to_32bit_unsigned(width_raw)))
}

#' @describeIn figure_dimensions Get 32-bit unsigned value
#'
#' @param raw_32bit A raw vector of length 32 bits (4 bytes)
#'
#' @export
raw_to_32bit_unsigned <- function(raw_32bit) {
  if(!( inherits(raw_32bit, 'raw') & length(raw_32bit) == 4)) {
    rlang::abort('raw_32bit must be a 32-bit raw vector')
  }
  dim_in_bits = rawToBits(raw_32bit)
  v = 0
  for(i in 1:length(dim_in_bits)) {
    v = v + 2^(i-1)*as.integer(dim_in_bits[i])
  }
  return(v)
}
