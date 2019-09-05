#' Summarise y values over binned wind data, split into u and v components.
#'
#' Input data should be original unbinned data including wind direction and wind velocity;
#' binning is done 2-dimensional over cartesian u and v wind vectors
#'
#' @param data a data.frame or tibble containing the data (wide format).
#' requires input data including at least three columns carrying information regarding:
#' * wind direction (in °)
#' * wind velocity
#' * z-values (e.g. air pollutant concentration)
#' @param ws symbol giving the wind velocity parameter name (wind velocity preferably in m/s)
#' @param wd symbol giving the wind direction parameter name  in degrees
#' @param z symbol giving the parameter name to be summarised
#' @param groupings additional groupings. Use helper [groups()] to create
#' @param fun function or list of functions for summary.
#' @param fun.args a list of extra arguments to pass to fun.
#' @param nmin numeric, minimum number of values for fun, if n < nmin: NA is returned
#' @param ws_max numeric or NA, maximum wind velocity for binning: above ws_max, z is set NA
#' @param bins numeric, number of bins over the range of values if `!groups %in% c("u", "v")`
#' @param smooth TRUE/FALSE, applies if groups = c("u", "v"); should smoothing of summary results should be performed
#' using [fit_gam_surface()]?
#' @param k numeric, applies if smooth = TRUE; degree of smoothing in smooth term in [fit_gam_surface()]
#' @param extrapolate TRUE/FALSE, applies if smooth = TRUE; [fit_gam_surface()] returns extrapolated (predicted) values for u, v coordinates that otherwise would have have NA for summarised z
#' if extrapolate = TRUE, those values are returned (to a certain degree depending on the value of dist)
#' @param dist numeric, fraction of 1, applies if smooth = TRUE and extrapolate = TRUE; maximum distance to next coordinate-pair at which the result of
#' fit_gam_surface(z) should be returned
#'
#' @return a tibble with summarised data along u and v wind vectors
#'
#' @section Computed variables:
#'
#' * a tibble is returned, binned over u and v, with variables:
#' - wd: wind direction corresponding to midpoint value of u and v
#' - ws: wind velocity corresponding to midpoint value of u and v
#' - u: midpoints of bins over u (from input wd and ws)
#' - v: midpoints of bins over v (from input wd and ws)
#' - z: result from fun(z, ...)
#'
#' @export
#'
#' @examples
#' fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_2010-2014.csv")
#' data <- rOstluft::read_airmo_csv(fn)
#' data <- rOstluft::rolf_to_openair(data)
#'
#' # summary NO2
#' summary_wind_2d(data, ws, wd, NO2, smooth = FALSE)
#'
#' # multiple stats: Pass function, by name, reference, as function or one sided formula
#' funs <- list(
#'   "mean",
#'   "median" = function(x) median(x, na.rm = TRUE),
#'   "q95" = ~ stats::quantile(., probs = 0.95)
#' )
#'
#' summary_wind_2d(data, ws, wd, NO2, fun = funs, smooth = FALSE)
#'
#' # is for some reason fun.args used with multiple functions, use ... to catch
#' # superfluous arguments:
#' funs <- list(
#'   "q95" = function(x, ...) stats::quantile(x, probs = 0.95),
#'   "mean"
#' )
#'
#' summary_wind_2d(data, ws, wd, NO2, fun = funs, fun.args = list(na.rm = TRUE),
#'                 smooth = FALSE)
#'
#' # additional groupings
#' summary_wind_2d(data, ws, wd, NO2, groupings = groups(site), smooth = FALSE)
#'
#' # we can use expressions in groups. For better readability groupings is
#' # defined outside of the function call
#' groupings = groups("site", year = lubridate::year(date))
#'
#' summary_wind_2d(data, ws, wd, NO2, groupings = groupings, smooth = FALSE)
#'
#' # smoothing
#' df1 <- summary_wind_2d(data, ws, wd, NO2, bins = 100^2, smooth = TRUE, k = 100)
#' df2 <- summary_wind_2d(data, ws, wd, NO2, bins = 100^2, smooth = FALSE)
#'
#' df <- dplyr::bind_rows(smoothed = df1, raw = df2, .id = "smoothing")
#'
#' ggplot(df, aes(x = u, y=v, fill=NO2)) +
#'   geom_raster() +
#'   scale_fill_viridis_c(na.value = NA) +
#'   facet_wrap(vars(smoothing))
#'
#' # for a small number of bins reduce k
#' summary_wind_2d(data, ws, wd, NO2, bins = 5^2, smooth = TRUE, k = 5)
summary_wind_2d <- function(data, ws, wd, z, groupings = groups(), fun = "mean", fun.args = list(), nmin = 3, ws_max = NA,
                            bins = 10^2, smooth = TRUE, k = 100, extrapolate = TRUE, dist = 0.1) {

  ws <- rlang::ensym(ws)
  wd <- rlang::ensym(wd)
  z <- rlang::ensym(z)

    # rename z if needed. we can't apply summarize functions on grouping columns!
  # for ws and wd we do auto renaming.
  if (ws == z) {
    z <- rlang::sym(stringr::str_c(rlang::as_string(ws), ".stat"))
    data <- dplyr::mutate(data, !!z := !!ws)
  }

  if (wd == z) {
    z <- rlang::sym(stringr::str_c(rlang::as_string(wd), ".stat"))
    data <- dplyr::mutate(data, !!z := !!wd)
  }

  fun <- auto_name(c(fun, "n" = function(x, ...) {sum(!is.na(x))}))

  data <- dplyr::mutate(data,
      u = !!ws * sin(pi * !!wd / 180),
      v = !!ws * cos(pi * !!wd / 180)
  )

  # shouldn't it be ws_max / sqrt(2) ??
  uv_max <- pmin(max(abs(c(data$u, data$v)), na.rm = TRUE), ws_max, na.rm = TRUE)
  nbins <- round(sqrt(bins), 0)
  binwidth <- 2 * uv_max / nbins
  uv_breaks <- seq(-uv_max, by = binwidth, length.out = nbins + 1)

  data <- dplyr::mutate(data,
    u = cut(u, breaks = uv_breaks, include.lowest = TRUE),
    v = cut(v, breaks = uv_breaks, include.lowest = TRUE)
  )

  # filter NAs, ith stats::na.omit() every row containing a NA value will be removed
  data <- dplyr::filter(data, !(is.na(u) | is.na(v) | is.na(!!z)))

   # apply the summarize function regarding the addiotional grouping columns
  data <- dplyr::group_by(data, .data$u, .data$v, !!!groupings)

  data <- dplyr::summarise_at(data,
    .vars = dplyr::vars(!!z),
    .funs = fun,
    !!!fun.args
  )
  data <- dplyr::ungroup(data)

  # calculate freqency per groupings
  data <- dplyr::group_by(data, !!!rlang::syms(names(groupings)))
  data <- dplyr::mutate(data, freq = .data$n / sum(.data$n, na.rm = TRUE))
  data <- dplyr::ungroup(data)

  data <- dplyr::filter(data, n >= nmin)
  data <- tidyr::gather(data, "stat", !!z, -u, -v, -n, -freq,
                        -dplyr::one_of(names(groupings)))

  # ensure that for every combination of u, v, stat and groupings a value is present
  # predict in fit_gam_surface needs a complete grid
  # geom_raster doesn't like missing raster points
  cols <- c("u", "v", "stat", names(groupings))
  data <- tidyr::complete(data, !!!rlang::syms(cols), fill = list(n = 0, freq = 0))

  # convert the uv factors to the numeric mid point
  data <- dplyr::mutate(data,
    u = -uv_max + binwidth * as.numeric(.data$u) - binwidth / 2,
    v = -uv_max + binwidth * as.numeric(.data$v) - binwidth / 2
  )

  if (isTRUE(smooth)) {
    data <- dplyr::group_by(data, .data$stat, !!!rlang::syms(names(groupings)))
    data <- dplyr::group_modify(data,
      ~ fit_gam_surface(.x, x = "u", y = "v", z = z, weights = pmin(3, .x$n) / 3,
                        k = k, extrapolate = extrapolate, dist = dist)
    )
  }

  # calculate wd and ws for the midpoints of uv
  data <- dplyr::mutate(data,
    !!wd := uv2wd(.data$u, .data$v),
    !!ws := sqrt(.data$u^2 + .data$v^2)
  )

  return(data)
}
