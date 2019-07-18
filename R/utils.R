

#' calculate wind direction from u, v wind components
#'
#' @export
uv2wd <- function(u, v) { #' von hier: https://github.com/environmentalinformatics-marburg/Rsenal/blob/master/R/uv2wdws.R
  degrees <- function(radians) 180 * radians / pi
  mathdegs <- degrees(atan2(v, u))
  wdcalc <- ifelse(mathdegs > 0, mathdegs, mathdegs + 360)
  wd <- ifelse(wdcalc < 270, 270 - wdcalc, 270 - wdcalc + 360)
  return(wd)
}




#' set up a bounding box for ggmap::get_map()
#'
#' @export
bbox <- function(bb) {
  c("left" = bb$lon[bb$type == "lb"], "bottom" = bb$lat[bb$type == "lb"], "right" = bb$lon[bb$type == "rt"], "top" = bb$lat[bb$type == "rt"])
}


#' Wrapper for sprintf string formatting
#'
#' Can be used to provide format functions for labels in ggpltos
#'
#' @param fmt sprintf formatting string. see `sprintf()`
#'
#' @return function formatting x with sprintf
#' @export
#'
#' @examples
#' # add one leading zeroes for 1 digits numbers
#' add_leading_zeroes <- format_sprintf("%02d")
#' add_leading_zeroes(1:10)
format_sprintf <- function(fmt) {
  function(x) {
    sprintf(fmt, x)
  }
}


