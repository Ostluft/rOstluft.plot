# require(suncalc)

fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_2010-2014.csv")

data <-
  rOstluft::read_airmo_csv(fn) %>%
  rOstluft::resample(new_interval = "h1")

groupings <- c("starttime_of_day")
fun = list("mean", "sd", "median", "mad", "quantile05", "quantile25", "quantile75", "quantile95")
y <- "value"
fun.args = list(na.rm = TRUE)
nmin = 3






quantile02 <- function(x, ...) quantile(x, 0.02, ...)
quantile05 <- function(x, ...) quantile(x, 0.05, ...)
quantile25 <- function(x, ...) quantile(x, 0.25, ...)
quantile75 <- function(x, ...) quantile(x, 0.75, ...)
quantile95 <- function(x, ...) quantile(x, 0.95, ...)
quantile98 <- function(x, ...) quantile(x, 0.98, ...)


cut_timeseries <- function(data, coord = c(lat = 47.36667, lon = 8.55)) {

  data <-
    dplyr::mutate(data,
                  starttime_of_day = format(starttime[1] + lubridate::hours(lubridate::hour(starttime)) + lubridate::minutes(lubridate::minute(starttime)), "%H:%M"),
                  date = lubridate::as_date(starttime),
                  weekday = lubridate::wday(starttime, label = TRUE, week_start = 1),
                  week = lubridate::week(starttime),
                  month = lubridate::month(starttime, label = TRUE)
    )
    data <- openair::cutData(data, type = "season")
    data <- openair::cutData(data, type = "weekend")

  suntime <- suncalc::getSunlightTimes(date = unique(lubridate::as_date(data$starttime)), lat = coord["lat"], lon = coord["lon"],
                                       tz = lubridate::tz(data$starttime), keep = c("sunrise", "sunset"))

  data <- dplyr::right_join(data, dplyr::select(suntime, date, sunrise, sunset), by = "date")
  data <- dplyr::mutate(data,
                        daylight = dplyr::case_when(
                          starttime > sunrise ~ "day",
                          starttime <= sunset ~ "night"
                        )
  )
  data <- dplyr::mutate_if(data, is.character, factor)
}


summary_periodic <- function(

  data,
  y = "value",
  groupings = "starttime_of_day",
  fun = list("mean", "sd", "median", "mad", "quantile05", "quantile25", "quantile75", "quantile95"),
  fun.args = list(na.rm = TRUE),
  nmin = 3,
  coord = c(lat = 47.36667, lon = 8.55) # Zürich

) {

  fun <- auto_name(c(fun, "n" = function(...) dplyr::n()))
  groupings <- c(groupings, "site", "parameter", "interval", "unit")
  summary_groups <- groups(!!!groupings)
  not_gather_groups <- c(names(summary_groups), "n", "freq")

  data <- cut_timeseries(data)

  # apply the summarize function regarding the addiotional grouping columns
  data <- dplyr::group_by(data, !!!summary_groups)
  data <- dplyr::summarise_at(data,
                              .vars = dplyr::vars(!!y),
                              .funs = fun,
                              !!!fun.args
  )
  data <- dplyr::ungroup(data)

  # calculate frequencies for each groupings
  data <- dplyr::group_by(data, !!!rlang::syms(names(groupings)))
  data <- dplyr::mutate(data, freq = .data$n / sum(.data$n, na.rm = TRUE))
  data <- dplyr::ungroup(data)

  data <- tidyr::gather(data, key = "stat", value = !!y, -dplyr::one_of(not_gather_groups))

  # factorize stat column
  data <- dplyr::mutate(data, stat = factor(stat))
  data <- dplyr::filter(data, n >= nmin)


  return(data)
}










