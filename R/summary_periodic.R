
#' Summarise a [rOstluft::format_rolf()] dataset over periodic (diurnal, weekday, etc) factors
#' implicitly created by [cut_timeseries_periodic()] using various stat functions.
#'
#' Input data should be [rOstluft::format_rolf()] data.
#'
#' @param data a data.frame or tibble containing the data
#' @param y a character string specifying the taget variable to be summarised, usually "value".
#' @param groupings additional groupings. Use helper [grp()] to create; groupings must be from  possible
#' outcomes of [rOstluft::cut_timeseries_periodic()].
#' @param fun function or list of functions for summary, can be named (then the outut stat is named after the function's name);
#'   Strings matching the regular expression `/^percentile([0-9]){1,2}$/i` are converted into the respective function.
#'   "percentile95" => `function(x, ...) quantile(x, 95 / 100, ...)`
#' @param fun.args a list of extra arguments passed on to fun.
#' @param nmin numeric, minimum number of values for fun, if n < nmin: NA is returned
#' @param coords a named vector of the location in  WGS84 coordinates for the daylight factoring.
#'   [suncalc::getSunlightTimes()] is used to calculate sunrise, sunset times.
#'   Default: c(lat = 47.36667, lon = 8.55) => Zuerich, Switzerland
#'
#' @return a tibble with summarised data
#'
#' @section Computed variables: as determined by [cut_timeseries_periodic()] in combination with `grouping`
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
#'   rOstluft::pluck_parameter("NOx", "O3", "PM10") %>%
#'   rOstluft::resample(new_interval = "h1")
#'
#' fun <- list(middle = "mean", middle2 = "median", ymin = "percentile05",
#'            lower = "percentile25", upper = "percentile75", ymax = "percentile95")
#'
#' data_summarized <- summary_periodic(data, fun = fun)
#' data_summarized
summary_periodic <- function(
  data,
  y = "value",
  groupings = grp("starttime_of_day"),
  fun = list("mean", "sd", "median", "mad", "percentile05", "percentile25", "percentile75", "percentile95"),
  fun.args = list(na.rm = TRUE),
  nmin = 3,
  coords = c(lat = 47.36667, lon = 8.55) # Zürich
) {

  fun <- auto_name(c(fun, "n" = function(...) dplyr::n()))
  fun <- create_percentile_functions(fun)

  summary_groups <- c(groupings, grp("site", "parameter", "interval", "unit"))
  not_gather_groups <- c(summary_groups, grp("n", "freq"))

  data <- cut_timeseries_periodic(data)

  # apply the summarize function regarding the addiotional grouping columns
  data <- dplyr::group_by(data, !!!summary_groups)
  data <- dplyr::summarise_at(data,
    .vars = dplyr::vars(y),
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
  data <- dplyr::mutate(data, stat = factor(.data$stat))
  data <- dplyr::filter(data, .data$n >= nmin)


  return(data)
}


#' Searches a list for strings percentileXX and returns function (x, ...) quantile(x, XX, ...)
#'
#' @param fun named list containing functions or strings
#'
#' @return named list with strings matching percentileXX replaced with functions
create_percentile_functions <- function(fun) {
  perc_regexp <- stringr::regex("^percentile([0-9]{1,2})$", ignore_case = TRUE)

  parse_percentiles <- function(x) {
    percentile <- stringr::str_match(x, perc_regexp)[2] # [1] is complete match, second is group
    function(x, ...) quantile(x, as.numeric(percentile) / 100, ...)
  }

  fun_names <- rlang::names2(fun)
  percs <- purrr::map_lgl(fun, ~is.character(.) && stringr::str_starts(., perc_regexp))
  percs_fun <- purrr::map(fun[percs], parse_percentiles)
  fun[percs] <- percs_fun
  return(rlang::set_names(fun, fun_names))
}












