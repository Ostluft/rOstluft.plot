#' wrapper to cut wind direction into factor classes
#'
#' @export
wd_classes <- function(wd, binwidth = 45, ...) { # in helpers verschieben
  stopifnot((360 / binwidth) %in% c(4, 8, 12, 16))
  if ((360 / binwidth) %in% c(4, 8, 12, 16)) {
    wd <- (wd + binwidth / 2) %% 360
  }
  wd <- ggplot2::cut_width(wd, width = binwidth, closed = "left", boundary = 0, ...)
  return(wd)
}

#' wrapper to cut wind direction into factor classes
#'
#' @export
cut_wd.fun <- function(binwidth = 45, ...) { # in helpers verschieben
  stopifnot((360 / binwidth) %in% c(4, 8, 12, 16))
  function(wd) {
    if ((360 / binwidth) %in% c(4, 8, 12, 16)) {
      wd <- (wd + binwidth / 2) %% 360
    }
    ggplot2::cut_width(wd, width = binwidth, closed = "left", boundary = 0, ...)
  }
}



#' wrapper to cut wind velocity (or others) into factor classes
#'
#' @export
ws_classes <- function(ws, binwidth = 1, ws_max = NA, ...) {
  ws <- cut(ws, breaks = seq(0, max(pmin(ws, ws_max, na.rm = TRUE), na.rm = TRUE), binwidth), ...)
  levels(ws) <- rev(levels(ws))
  return(ws)
}


#' wrapper to cut wind velocity (or others) into factor classes
#'
#' @export
cut_ws.fun <- function(binwidth = 1, ws_max = NA, ...) {
  function(ws) {
    ws <- cut(ws, breaks = seq(0, max(pmin(ws, ws_max, na.rm = TRUE), na.rm = TRUE), binwidth), ...)
    levels(ws) <- rev(levels(ws))
    return(ws)
  }
}


#' wrapper to cut y data into factor classes
#'
#' @export
y_classes <- function(y, binwidth, ymax = NA, boundary = 0, ...) {
  y <- ggplot2::cut_width(pmin(y, ymax, na.rm = TRUE), width = binwidth, boundary = boundary, ...)
  levels(y) <- rev(levels(y))
  return(y)
}
