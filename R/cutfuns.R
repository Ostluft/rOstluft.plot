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
cut_wd <- function(wd, binwidth = 45, add_labels = TRUE, ...) { # in helpers verschieben
  nsectors <- 360 / binwidth
  stopifnot(nsectors %in% c(4, 8, 12, 16))
  # ll <- c("N", "NNO", "NO", "NOO", "O", "SOO", "SO", "SSO",
  #         "S", "SSW", "SW", "SWW", "W", "NWW", "NW", "NNW")

  if (isTRUE(add_labels)) {
    labels <- seq(0, 359, binwidth)
  } else {
    labels <- NULL
  }

  wd <- (wd + binwidth / 2) %% 360

  ggplot2::cut_width(wd, width = binwidth, closed = "left", boundary = 0, labels = labels, ...)
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
  function(wd) {
    cut_wd(wd, binwidth = binwidth, ...)
  }
}



#' Cut wind velocity (or others) into factor classes
#'
#' Based on [base::cut()]. Allows to specifiy maximum wind velocity. If `squish = TRUE` the values greater than `ws_max`
#' will be combined to one additional factor level ">ws_max". If `squish = FALSE` the resulting vector will contain
#' NA for this values. The correct handling of the NA values in the factor must be done by the user.
#'
#' @param ws numeric vector of wind velocity
#' @param binwidth width of the bins
#' @param ws_max cut off wind speed at this maximum
#' @param squish If TRUE wind velocities greater than will be include as additional level ">ws_max"
#' @param right logical, indicating if the intervals should be closed on the right (and open on the left) or vice versa.
#' @param reverse reverse order of result. This is sometimes useful when plotting a factor.
#'
#' @export
#'
#' @examples
#' ws <- c(0.5, 1.1, 2.2, 3.3, 4.4, 5, 8.8)
#'
#' cut_ws(ws, binwidth = 2)
#'
#' # if ws_max not a multiple of binwidth, the last level before squishing will be cut short
#' cut_ws(ws, binwidth = 2, ws_max = 5)
#'
#' cut_ws(ws, binwidth = 2, ws_max = 5, squish = FALSE)
#'
#' # close the intervals on the left side
#' cut_ws(ws, binwidth = 2, ws_max = 5, right = FALSE)
#'
#' # reverse the order of the factors, useful for legends while plotting
#' cut_ws(ws, binwidth = 2, ws_max = 5, reverse = TRUE)
cut_ws <- function(ws, binwidth = 1, ws_max = NA, squish = TRUE, right = TRUE, reverse = FALSE) {
  last_label <- NULL

  # find the last cut point. Must be the first multiple of binwidth which is greater then
  # maximum wind_speed
  if (is.na(ws_max)) {
    ws_max_data <- max(ws, na.rm = TRUE)
    last_cut_point <- ceiling(ws_max_data / binwidth) * binwidth
    breaks <- seq(0, last_cut_point, binwidth)
  } else {
    # ensure ws_max is included for the case, ws_max isn't a mulitple of binwidth
    breaks <- unique(c(seq(0, floor(ws_max / binwidth) * binwidth, binwidth), ws_max))

    # do we need to squish the data?
    if (isTRUE(squish)) {
      last_label <- sprintf(ifelse(isTRUE(right), ">%s", "\U2265%s"), utils::tail(breaks, 1))
      breaks <- c(breaks, Inf)
    }
  }

  ws <- cut(ws, breaks = breaks, right = right, include.lowest = TRUE)

  if (!is.null(last_label)) {
    levels(ws)[length(levels(ws))] <- last_label
  }


  if (isTRUE(reverse)) ws <- forcats::fct_rev(ws)

  return(ws)
}


#' Partial function constructor to cut wind velocity (or others) into factor classes
#'
#' @inheritParams cut_ws
#'
#' @return a partial [cut_ws()] function with ws as sole argument
#'
#' @export
cut_ws.fun <- function(binwidth = 1, ws_max = NA, squish = TRUE, right = TRUE, reverse = FALSE) {
  function(ws) {
    cut_ws(ws, binwidth, ws_max, squish, right, reverse)
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
