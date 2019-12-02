
#' Summarise a [rOstluft::format_rolf()] dataset over periodic (diurnal, weekday, etc) factors
#' implicitly created by [rOstluft::cut_timeseries_periodic()] using various stat functions.
#'
#' Input data should be [rOstluft::format_rolf()] data.
#'
#' @param data a data.frame or tibble containing the data (wide format).
#' @param y a character string specifying the taget variable to be summarised, usually "value".
#' @param groupings additional groupings. Use helper [grp()] to create; groupings must be from  possible
#' outcomes of [rOstluft::cut_timeseries_periodic()].
#' @param fun function or list of functions for summary, can be named (then the outut stat is named after the function's name);
#' pre-defined quantile-stat-functions can be directly included: "quantile01","quantile02", "quantile05", "quantile10", "quantile25",
#' "quantile75", "quantile90", "quantile95","quantile98","quantile99".
#' @param fun.args a list of extra arguments passed on to fun.
#' @param nmin numeric, minimum number of values for fun, if n < nmin: NA is returned
#' @param coord_sun anamed vector von lat / lon coordinates (WGS84) of the location for which
#' to calculate sunrise / sunset times (per default: c(lat = 47.36667, lon = 8.55) => Zürich, Switzerland)
#' for daylight factoring using [suncalc::getSunlightTimes()].
#'
#' @return a tibble with summarised data
#'
#' @section Computed variables: as determined by [rOstluft::cut_timeseries_periodic()] in combination with `grouping`
#'
#' The names of the columns stay the same as in `data`.
#'
#' * factor columns according to the groups specified in `grouping`
#' * factor column `stat` containing the name of the summarize function as level
#' * column `value` with the result of the summarize function
#' * `n` count of occurrences for the corresponding bin
#' * `freq` frequency of the corresponding bin
#'
#' @export
#'
#' @examples
#' fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_2010-2014.csv")
#'
#' data <-
#'   rOstluft::read_airmo_csv(fn) %>%
#'   dplyr::filter(parameter %in% c("NOx", "O3", "PM10")) %>%
#'   rOstluft::resample(new_interval = "h1")
#'
#' fun <- list(middle = "mean", middle2 = "median", ymin = "quantile05",
#'            lower = "quantile25", upper = "quantile75", ymax = "quantile95")
#'
#' data_summarized <- summary_periodic(data, y = "value", groupings = grp(starttime_of_day), fun = fun,  nmin = 3)
#' data_summarized
summary_periodic <- function(

  data,
  y = "value",
  groupings = grp(starttime_of_day),
  fun = list("mean", "sd", "median", "mad", "quantile05", "quantile25", "quantile75", "quantile95"),
  fun.args = list(na.rm = TRUE),
  nmin = 3,
  coord_sun = c(lat = 47.36667, lon = 8.55) # Zürich

) {


  quantile01 <- function(x, ...) quantile(x, 0.01, ...)
  quantile02 <- function(x, ...) quantile(x, 0.02, ...)
  quantile05 <- function(x, ...) quantile(x, 0.05, ...)
  quantile10 <- function(x, ...) quantile(x, 0.1, ...)
  quantile25 <- function(x, ...) quantile(x, 0.25, ...)
  quantile75 <- function(x, ...) quantile(x, 0.75, ...)
  quantile90 <- function(x, ...) quantile(x, 0.90, ...)
  quantile95 <- function(x, ...) quantile(x, 0.95, ...)
  quantile98 <- function(x, ...) quantile(x, 0.98, ...)
  quantile99 <- function(x, ...) quantile(x, 0.99, ...)

  fun <- auto_name(c(fun, "n" = function(...) dplyr::n()))
  summary_groups <- c(groupings, grp(site, parameter, interval, unit))
  not_gather_groups <- c(summary_groups, grp(n, freq))

  data <- cut_timeseries_periodic(data)

  # apply the summarize function regarding the addiotional grouping columns
  data <- dplyr::group_by(data, !!!summary_groups)
  data <- dplyr::summarise_at(data,
                              .vars = dplyr::vars(!!y),
                              .funs = fun,
                              !!!fun.args
  )
  data <- dplyr::ungroup(data)

  # calculate frequencies for each groupings
  data <- dplyr::group_by(data, !!!groupings)
  data <- dplyr::mutate(data, freq = .data$n / sum(.data$n, na.rm = TRUE))
  data <- dplyr::ungroup(data)

  data <- tidyr::gather(data, key = "stat", value = !!y, -dplyr::one_of(names(not_gather_groups)))

  # factorise stat column
  data <- dplyr::mutate(data, stat = factor(stat))
  data <- dplyr::filter(data, n >= nmin)


  return(data)
}









