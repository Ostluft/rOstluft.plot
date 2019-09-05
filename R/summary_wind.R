#' Summarise z values over binned wind data.
#'
#' Input data should be original unbinned data.
#' 1-dimensional binning and calculating summary statistics over wind direction and/or wind velocity bins, respectively.
#' NA values in z and in ws, wd (after cutting) will be silently removed before applying functions
#'
#' @param data a data.frame or tibble containing the data (wide format)
#' @param ws symbol giving the wind velocity parameter name (wind velocity preferably in m/s)
#' @param wd symbol giving the wind direction parameter name  in degrees
#' @param z symbol giving the parameter name to be summarised
#' @param groupings additional groupings. Use helper [groups()] to create
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
#' The names of the columns stay the same as in `data` (for the arguments "ws", "wd" and "z").
#'
#' * binned column `ws`
#' * binned column `wd`
#' * factor column `stat` containing the name of the summarize function as level
#' * column `z` with the result of the summarize function
#' * `n` count of occurrences for the corresponding bin
#' * `freq` frequency of the corresponding bin
#'
#' @export
#'
#' @examples
#' fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_2010-2014.csv")
#' data <- rOstluft::read_airmo_csv(fn)
#' data <- rOstluft::rolf_to_openair(data)
#' data <- dplyr::mutate(data, year = lubridate::year(date))
#'
#' summary_wind(data, "ws", "wd", "NO2")
#'
#' # multiple stats: Pass function, by name, reference, as function or one sided formula
#' q95 <- function(x) stats::quantile(x, probs = 0.95)
#'
#' funs <- list(
#'   "mean",
#'   "median",
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
#'   dplyr::select(q95_1, q95_2, q95_3)
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
#' # only one ws class ()
#' summary_wind(data, "ws", "wd", "NO",
#'              ws_cutfun = cut_number.fun(1))
#'
#' # additional grouping with strings, symbols or named expressions
#' summary_wind(data, ws, wd, NO2, group = groups("site", year, wday = lubridate::wday(date)))
#'
#' # how often comes which concentration from one direction
#' summary_wind(data, ws, wd, NO2,
#'              group = groups(NO2_class = ggplot2::cut_number(NO2, 5)),
#'              ws_cutfun = cut_number.fun(1))
#'
#' # the same but we use ws as pollutant
#' summary_wind(data, NO2, wd, NO2, ws_cutfun = cut_number.fun(5))
summary_wind <- function(data, ws, wd, z, groupings = groups(), fun = "mean", fun.args = list(), nmin = 3,
                          wd_cutfun = cut_wd.fun(binwidth = 45),
                          ws_cutfun = cut_ws.fun(binwidth = 1)) {

  wd <- rlang::ensym(wd)
  ws <- rlang::ensym(ws)
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
  # names <- purrr::map2(fun, rlang::names2(fun), function(element, name) {if (name != "") name else element})
  # fun <- rlang::set_names(fun, names)

  # apply binning trough cut fuctions
  data <- dplyr::mutate(data, !!wd := wd_cutfun(!!wd), !!ws := ws_cutfun(!!ws))

  # with stats::na.omit() every row containing a NA value will be filtered
  data <- dplyr::filter(data, !(is.na(!!wd) | is.na(!!ws) | is.na(!!z)))

 # apply the summarize function regarding the addiotional grouping columns
  data <- dplyr::group_by(data, !!wd, !!ws, !!!groupings)
  data <- dplyr::summarise_at(data,
      .vars = dplyr::vars(!!z),
      .funs = fun,
      !!!fun.args
  )
  data <- dplyr::ungroup(data)

  if (length(groupings) > 0) {
    data <- dplyr::group_by(data, !!!rlang::syms(names(groupings)))
    data <- dplyr::mutate(data, freq = .data$n / sum(.data$n, na.rm = TRUE))
    data <- dplyr::ungroup(data)
    data <- tidyr::gather(data, key = "stat", value = !!z, -!!wd, -!!ws, -n, -freq,
                          -dplyr::one_of(names(groupings)))
  } else {
    data <- dplyr::mutate(data, freq = .data$n / sum(.data$n, na.rm = TRUE))
    data <- tidyr::gather(data, key = "stat", value = !!z, -!!wd, -!!ws, -n, -freq)
  }

  # factorize stat column
  data <- dplyr::mutate(data, stat = factor(stat))
  data <- dplyr::filter(data, n >= nmin)

  return(data)
}
