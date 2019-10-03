#' Cut wind direction into factor classes
#'
#' @description Wraps [ggplot2::cut_width()] function with `width = binwidth. closed = "left", boundary = 0`
#' as fixed arguments
#'
#' @param wd numeric vector of wind directions in °
#' @param binwidth width for [ggplot2::cut_width()] in degrees wind  direction
#' (must fullfill binwidth \%in\% 360 / c(4, 8, 16, 32))
#' @param labels character vector as labels for wind direction bins; can be NULL (no labels are returned),
#' if !is.null(labels) then length(labels) == 32 must be fullfilled (actual labels are subsampled with
#' indices of seq(1, length(labels), length(labels) / nsectors))
#' @param ... passed to [ggplot2::cut_width()]
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' wd <- seq(0, 359)
#' ws <- runif(length(wd))
#'
#' # helper function to visualise cuts
#' plot_cut <- function(wd, ws) {
#'   nSectors <- length(levels(wd))
#'   data <- tibble::tibble(wd = wd, ws = ws) %>%
#'     dplyr::group_by(wd) %>%
#'     dplyr::summarise(ws = mean(ws))
#'
#'   ggplot(data, aes(x = wd, y = ws)) +
#'     geom_bar(stat = "identity", width = 1, color = "blue", size = 1, alpha = 0.5) +
#'     coord_polar2(start = - pi / nSectors) +
#'     scale_x_discrete(expand = c(0, 0))
#' }
#'
#'
#' # defaults
#' plot_cut(cut_wd(wd), ws)
#'
#' # maximum number of cuts
#' plot_cut(cut_wd(wd, binwidth = 11.25), ws)
#'
#' # maximum number of cuts with text labels
#' plot_cut(cut_wd(wd, binwidth = 22.5), ws)
#'
#' # change label text
#' labels <- c("North", "East", "South", "West")
#' plot_cut(cut_wd(wd, binwidth = 90, labels = labels), ws)
cut_wd <- function(wd, binwidth = 45,
                   labels = c("N", "[5.6,16.9)", "NNO", "[28.1,39.4)", "NO", "[50.6,61.2)", "NOO", "[73.1,84.4)", "O", "[95.6,106.9)",
                              "SOO", "[118.1,129.4)", "SO", "[140.6,151.9)", "SSO", "[163.1,174.4)", "S", "[185.6,196.9)", "SSW",
                              "[208.1,219.4)", "SW", "[230.6,241.9)", "SWW", "[253.1,264.)", "W", "[275.6,286.9)", "NWW",
                              "[298.1,309.4)", "NW", "[320.6,331.9)", "NNW", "[343.1,354.4)"),
                   ...) {
seq(11.25 / 2, 360-11.25/2, 11.25)
  nsectors <- 360 / binwidth
  stopifnot(nsectors %in% c(4, 8, 16, 32))
  stopifnot(length(labels) %in% c(4, 8, 16, 32) | is.null(labels))

  if (!is.null(labels)) {
    labels <- labels[seq(1, length(labels), length(labels) / nsectors)]
  }

  ggplot2::cut_width(wd, width = binwidth, closed = "left", boundary = 0, labels = labels, ...)
}


#' Partial function constructor to cut wind direction into factor classes
#'
#' Creates a partial function of [ggplot2::cut_width()] with `width = binwidth. closed = "left", boundary = 0` as fixed
#' arguments
#'
#' @inheritParams cut_wd
#' @seealso [cut_wd()]
#'
#' @return a partial [cut_wd()] function with wd as sole argument
#'
#' @export
cut_wd.fun <- function(binwidth = 45, ...) {
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
#' # unfortunately there is a issue in converting the console output to
#' # html: the unicode character for >= gets scrambled to =
#' # https://github.com/r-lib/evaluate/issues/59
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
#' @seealso [cut_ws()]
#'
#' @return a partial [cut_ws()] function with ws as sole argument
#'
#' @export
cut_ws.fun <- function(binwidth = 1, ws_max = NA, squish = TRUE, right = TRUE, reverse = FALSE) {
  function(ws) {
    cut_ws(ws, binwidth, ws_max, squish, right, reverse)
  }
}




#' Partial function constructor for ggplot2 cut functions
#'
#' This Wrappers creates partial functions with x as sole argument of
#' the [ggplot2 cut functions][ggplot2::cut_interval()]
#'
#'
#' @inheritParams ggplot2::cut_interval
#' @seealso
#'   * [ggplot2::cut_interval()]
#'   * [ggplot2::cut_number()]
#'   * [ggplot2::cut_width()]
#'
#' @return function
#'
#' @export
#' @examples
#'
#' data <- tibble::tibble(x = runif(100, 0, 10))
#'
#' funs <- list(
#'   interval = cut_interval.fun(n = 5),
#'   number = cut_number.fun(n = 5),
#'   width = cut_width.fun(width = 2, boundary = 0)
#' )
#'
#' res <- dplyr::mutate_all(tibble::tibble(x = runif(100, 0, 10)), funs)
#'
#' res
#'
#' table(res$interval)
#'
#' table(res$number)
#'
#' table(res$width)
cut_interval.fun <- function(n = NULL, length = NULL, ...) {
  function(x) {
    ggplot2::cut_interval(x, n = n, length = length, ...)
  }
}

#' @inheritParams ggplot2::cut_number
#'
#' @export
#' @rdname cut_interval.fun
cut_number.fun <- function(n = NULL, ...) {
  function(x) {
    ggplot2::cut_number(x, n = n, ...)
  }
}


#' @inheritParams ggplot2::cut_width
#'
#' @export
#' @rdname cut_interval.fun
cut_width.fun <- function(width, center = NULL, boundary = NULL,
                          closed = c("right", "left"), ...) {
  function(x) {
    ggplot2::cut_width(x, width, center, boundary, closed, ...)
  }
}


#' Cut date-time vectors into seasons
#'
#' @param x a date-time vector
#' @param labels a list for recoding. Names and order should be "DJF", "MAM", "JJA", "SON"
#'
#' @return factor of seasons
#' @export
#'
#' @examples
#' dates <- lubridate::ymd(010101) + months(0:11)
#'
#' cut_season(dates)
#'
#' cut_season(dates, c(DJF = "winter", MAM = "spring", JJA = "summer", SON = "autumn"))
cut_season <- function(x, labels = NULL) {
  cc <- c(
    "DJF", "DJF", "MAM", "MAM", "MAM", "JJA",
    "JJA", "JJA", "SON" , "SON" , "SON", "DJF"
  )

  x <- ordered(cc[lubridate::month(x)], levels = c("DJF", "MAM", "JJA", "SON"))

  if (!is.null(labels)) {
    x <- dplyr::recode_factor(x, !!!labels, .ordered = TRUE)
  }
  x
}


#' Partial function constructor for cut_season
#'
#' @inheritParams cut_season
#' @seealso [cut_season()]
#'
#' @return Partial function of [cut_season()] with x as sole argument
#' @export
cut_season.fun <- function(labels = NULL) {
  function(x) {
    cut_season(x, labels = labels)
  }
}


#' Cut seasons, keep years together
#'
#' @description Cut the data in year-season intervals while keeping the seasons together.
#' This means december will be added to the following year.
#'
#' With `label = "year"` only the year will be adjustet.
#'
#' @param x a date-time vector
#' @param label choice between `c("yearseason", "year")`. `"yearseason"` will combine
#'   the year and the output from [cut_season()], `"year"` will return only the
#'   adjusted year.
#' @param labels forwarded to [cut_season()]
#'
#' @return factor of yearseasons
#' @export
#'
#' @examples
#' dates <- lubridate::ymd(010101) + months(0:11)
#'
#' cut_seasonyear(dates)
#'
#' cut_seasonyear(dates, "year")
#'
#' # customize season labels
#' labels =  c(
#'   DJF = "winter", JJA = "summer",
#'   MAM = "spring", SON = "autumn"
#' )
#'
#' cut_seasonyear(dates, labels = labels)
cut_seasonyear <- function(x, label = c("yearseason", "year"), labels = NULL) {
  label <- match.arg(label)

  out <- dplyr::if_else(
    lubridate::month(x) == 12,   # if december
    lubridate::year(x) + 1,      # add to following year's season
    lubridate::year(x)
  )

  if (label == "yearseason") {
    out <- stringr::str_c(out, "-", cut_season(x, labels = labels))
  }

  ordered(out, levels = unique(out))
}

#' Partial function constructor for cut_season
#'
#' @inheritParams cut_seasonyear
#' @seealso [cut_seasonyear()]
#'
#' @return Partial function of [cut_seasonyear()] with x as sole argument
#' @export
cut_seasonyear.fun <- function(label = c("yearseason", "year"), labels = NULL) {
  function(x) {
    cut_seasonyear(label = label, labels = labels)
  }
}





