#' wrapper to cut wind direction into factor classes
#' 
#' @export
wd_classes <- function(wd, wd_binwidth = 45, ...) { # in helpers verschieben
  stopifnot((360 / wd_binwidth) %in% c(4, 8, 12, 16))
  if ((360 / wd_binwidth) %in% c(4, 8, 12, 16)) {
    wd <- (wd + wd_binwidth / 2) %% 360
  }
  wd <- ggplot2::cut_width(wd, width = wd_binwidth, closed = "left", boundary = 0, ...)
  return(wd)
}



#' wrapper to cut wind velocity (or others) into factor classes
#' 
#' @export
ws_classes <- function(ws, ws_binwidth = 1, ws_max = NA, ...) {
  cut(ws, breaks = seq(0, max(pmin(ws, ws_max, na.rm = TRUE), na.rm = TRUE), ws_binwidth), ...)
}


