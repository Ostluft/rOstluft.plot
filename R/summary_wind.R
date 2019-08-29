#' Summarise z values over binned wind data.
#'
#' Input data should be original unbinned data.
#' 1-dimensional binning and calculating summary statistics over wind direction and/or wind velocity bins, respectively.
#' NA values in z will be silently removed before applying functions
#'
#' @param data a data.frame or tibble containing the data (wide format)
#' @param ws string giving the wind velocity parameter name (wind velocity preferably in m/s)
#' @param wd string giving the wind direction parameter name  in degrees
#' @param z string giving the parameter name to be summarised
#' @param groups one or more character string for additional grouping
#' @param fun function or list of functions for summary.
#' @param fun.args a list of extra arguments passed on to fun.
#' @param nmin numeric, minimum number of values for fun, if n < nmin: NA is returned
#' @param wd_cutfun function, cut function for wind direction (to create bins). See [wd_cut.fun()] for options.
#' @param ws_cutfun function, cut function for wind speed. See [ws_cut.fun()] for examples
#'
#' @return a tibble with summarised data
#'
#' @section Computed variables:
#'
#' * If groups = NULL: groups = "wd". In this case, bins are calculated over wind direction;
#'   a tibble including wd and summarised z is returned
#' * groups can be strings for other variables in data; then fun is applied over those;
#'   a tibble including groups and summarised z is returned
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_2010-2014.csv")
#' data <- rOstluft::read_airmo_csv(fn)
#' data <- rOstluft::rolf_to_openair(data)
#' data <- dplyr::mutate(data, year = lubridate::year(date))
#'
#' summary_wind(data, "ws", "wd", "NO2")
#'
#' # works with symbols too and additional grouping
#' summary_wind(data, ws, wd, NO2, groups = c("site", "year"))
#'
#' # multiple stats: Pass function, by name, reference, as function or one sided formula
#' q95 <- function(x) stats::quantile(x, probs = 0.95)
#'
#' funs <- list(
#'   "mean",
#'   "median",
#'   "q95", # this variant isn't recommend
#'   "q95_1" = q95,
#'   "q95_2" = function(x) stats::quantile(x, probs = 0.95),
#'   "q95_3" = ~ stats::quantile(., probs = 0.95)
#' )
#'
#' res <- summary_wind(data, "ws", "wd", "NO2", fun = funs)
#' res
#'
#' # and q95, q95_1, q95_2, q95_3 alle have the same value
#' tidyr::spread(res, "stat", "NO2") %>%
#'   dplyr::select(q95, q95_1, q95_2, q95_3)
#'
#' # is for some reason fun.args used with multiple functions, use ... to catch
#' # superfluous arguments:
#' funs <- list(
#'   "q95" = function(x, ...) stats::quantile(x, probs = 0.95),
#'   "mean"
#' )
#' summary_wind(data, "ws", "wd", "NO2", fun = funs, fun.args = list(na.rm = TRUE))
#'
#'
#' # more wd classes, less ws classes and squish ws
#' summary_wind(data, "ws", "wd", "NO",
#'              wd_cutfun = cut_wd.fun(binwidth = 22.5),
#'              ws_cutfun = cut_ws.fun(binwidth = 2, ws_max = 6))
#'
summary_wind <- function(data, ws, wd, z, groups = c(), fun = "mean", fun.args = list(), na.rm = TRUE, nmin = 3,
                          wd_cutfun = cut_wd.fun(binwidth = 45),
                          ws_cutfun = cut_ws.fun(binwidth = 1)) {

  wd <- rlang::ensym(wd)
  ws <- rlang::ensym(ws)
  z <- rlang::ensym(z)

  fun <- c(fun, "n" = function(x, ...) {sum(!is.na(x))})
  names <- purrr::map2(fun, rlang::names2(fun), function(element, name) {if (name != "") name else element})
  fun <- rlang::set_names(fun, names)

  data <- dplyr::mutate(data, !!wd := wd_cutfun(!!wd), !!ws := ws_cutfun(!!ws))

  # with stats::na.omit() every row containing a NA value will be filtered
  #

  if (isTRUE(na.rm)) {
    data <- dplyr::filter(data, !(is.na(!!wd) | is.na(!!ws) | is.na(!!z)))
  } else {
    data <- dplyr::filter(data, !(is.na(!!wd) | is.na(!!ws)))
  }


  data <- dplyr::group_by(data, wd, ws, !!!rlang::syms(groups))
  data <- dplyr::summarise_at(data,
      .vars = dplyr::vars(!!z),
      .funs = fun,
      !!!fun.args
  )
  data <- dplyr::ungroup(data)

  if (length(groups) > 0) { # probably a more elegant way, but i'm stupid
    data <- tidyr::gather(data, key = "stat", value = !!z, -!!wd, -!!ws,  -n, -dplyr::one_of(!!!groups))
  } else {
    data <- tidyr::gather(data, key = "stat", value = !!z, -!!wd, -!!ws,  -n)
  }

  data <- dplyr::mutate(data, stat = factor(stat), freq = n / sum(n, na.rm = TRUE))
  data <- dplyr::filter(data, n >= nmin)

  return(data)
}


summary_wind_ref <- function(data, ws, wd, z, groups = NULL, fun = "mean", fun.args = list(), nmin = 3,
                          wd_cutfun = cut_wd.fun(binwidth = 45), wd_offset = 0,
                          ws_cutfun = cut_ws.fun(binwidth = 1, ws_max = NA)) {

  if (is.null(groups)) groups <- wd
  fun <- c(as.list(fun), "n" = function(x, ...) {sum(!is.na(x))})
  names <- purrr::map2(fun, rlang::names2(fun), function(element, name) {if (name != "") name else element})
  fun <- rlang::set_names(fun, names)
  data <-
    data %>%
    dplyr::mutate(
      !!wd := wd_cutfun(!!rlang::sym(wd)),
      !!ws := ws_cutfun(!!rlang::sym(ws))
    ) %>%
    stats::na.omit() %>%
    dplyr::group_by_at(groups) %>%
    dplyr::summarise_at(
      .vars = z,
      .funs = fun,
      !!!fun.args
    ) %>%
    dplyr::ungroup()

  data <- data %>%
    tidyr::gather(key = "stat", value = !!z, -!!groups, -n) %>%
    dplyr::mutate(stat = factor(stat), freq = n / sum(n, na.rm = TRUE)) %>%
    dplyr::filter(n >= nmin)

  return(data)
}

