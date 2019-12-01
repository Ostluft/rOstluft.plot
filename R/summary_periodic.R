






summary_periodic <- function(

  data,
  y = "value",
  groupings = "starttime_of_day",
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
  groupings <- c(groupings, "site", "parameter", "interval", "unit")
  summary_groups <- rlang::syms(groupings)
  not_gather_groups <- c(as.character(summary_groups), "n", "freq")

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
  data <- dplyr::group_by(data, !!!rlang::syms(names(groupings)))
  data <- dplyr::mutate(data, freq = .data$n / sum(.data$n, na.rm = TRUE))
  data <- dplyr::ungroup(data)

  data <- tidyr::gather(data, key = "stat", value = !!y, -dplyr::one_of(not_gather_groups))

  # factorise stat column
  data <- dplyr::mutate(data, stat = factor(stat))
  data <- dplyr::filter(data, n >= nmin)


  return(data)
}









