#' Cut wind direction into factor classes
#'
#' Wraps [ggplot2::cut_width()] function with `width = binwidth. closed = "left", boundary = 0` as fixed
#' arguments
#'
#' @param wd numeric vector of wind directions in °
#' @param binwidth width for [ggplot2::cut_width()]
#' @param ... passed to [ggplot2::cut_width()]
#'
#' @export
cut_wd <- function(wd, binwidth = 45, ...) { # in helpers verschieben
  stopifnot((360 / binwidth) %in% c(4, 8, 12, 16))
  if ((360 / binwidth) %in% c(4, 8, 12, 16)) {
    wd <- (wd + binwidth / 2) %% 360
  }
  ggplot2::cut_width(wd, width = binwidth, closed = "left", boundary = 0, ...)
}

#' Partial function constructor to cut wind direction into factor classes
#'
#' Creates a partial function of [ggplot2::cut_width()] with `width = binwidth. closed = "left", boundary = 0` as fixed
#' arguments
#'
#' @param binwidth width for [ggplot2::cut_width()]
#' @param ... passed to [ggplot2::cut_width()]
#'
#' @return a partial [ggplot2::cut_width()] function with wd as sole argument
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



#' Cut wind velocity (or others) into factor classes
#'
#' Wraps [base::cut()] with `breaks = seq(0, max(pmin(ws, ws_max, na.rm = TRUE), na.rm = TRUE), binwidth)` as fixed
#' argument
#'
#' @param ws numeric vector of wind velocitiy
#' @param binwidth width of the bins
#' @param ws_max cut off wind speed at this maximum
#' @param reverse reverse order of result. This is sometimes useful when plotting a factor.
#' @param ... passed onto [base::cut()]
#'
#' @export
cut_ws <- function(ws, binwidth = 1, ws_max = NA, reverse = FALSE, ...) {
  ws <- cut(ws, breaks = seq(0, max(pmin(ws, ws_max, na.rm = TRUE), na.rm = TRUE), binwidth), ...)
    if (isTRUE(reverse)) ws <- forcats::fct_rev(ws)
  return(ws)
}


#' Partial function constructor to cut wind velocity (or others) into factor classes
#'
#' Creates a partial function of [base::cut()] with
#' `breaks = seq(0, max(pmin(ws, ws_max, na.rm = TRUE), na.rm = TRUE), binwidth)` as fixed argument
#' @param binwidth width of the bins
#' @param ws_max cut off wind speed at this maximum
#' @param reverse reverse order of result. This is sometimes useful when plotting a factor.
#' @param ... passed onto [base::cut()]
#'
#' @return a partial [base::cut()] function with ws as sole argument
#'
#' @export
cut_ws.fun <- function(binwidth = 1, ws_max = NA, reverse = FALSE, ...) {
  function(ws) {
    ws <- cut(ws, breaks = seq(0, max(pmin(ws, ws_max, na.rm = TRUE), na.rm = TRUE), binwidth), ...)
    if (isTRUE(reverse)) ws <- forcats::fct_rev(ws)
    return(ws)
  }
}


#' wrapper to cut y data into factor classes
#'
#' Wraps [ggplot2::cut_width()] function
#'
#' @param y a numeric vector
#' @param binwidth for [ggplot2::cut_width()]
#' @param ymax cut off at this maximum
#' @param boundary for [ggplot2::cut_width()]
#' @param ... passed to [ggplot2::cut_width()]
#'
#' @export
y_classes <- function(y, binwidth, ymax = NA, boundary = 0, ...) {
  y <- ggplot2::cut_width(pmin(y, ymax, na.rm = TRUE), width = binwidth, boundary = boundary, ...)
  levels(y) <- rev(levels(y))
  return(y)
}
