#' Pads data to complete year
#'
#' @description Generic padding function. Generates a sequence from `lubridate::floor(start_date, "year")`
#' to `lubridate::ceil(end_date, "year")`. The last point is excluded if `end_date != max(data[[date_col]])`.
#' Under the hood the heavy lifting is done by [tidyr::complete()].
#'
#' @param data input data
#' @param date_col column containing date information, every date should be unique
#' @param interval interval between two dates
#' @param fill A named list that for each variable supplies a single value to use instead of
#'   NA for missing combinations.
#' @param start_date optional start_date instead of `min(data[[date_col]])`
#' @param end_date optional start_date instead of `max(data[[date_col]])`
#'
#' @return padded data
#' @export
#'
#' @examples
#' fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_min30_2013_Jan.csv")
#' data <- rOstluft::read_airmo_csv(fn)
#' data <- rOstluft::rolf_to_openair(data)
#'
#' # last data point is at 2013-01-31 23:30:00
#' tail(data)
#'
#' # the site column get filled with NA, this could lead to problems
#' tail(pad_to_year(data, date, "30 min"))
#'
#' # better to provide a fill value, for more complex cases use pad_to_year_fill()
#' tail(pad_to_year(data, date, "30 min", fill = list(site = "Zch_Stampfenbachstrasse")))
pad_to_year <- function(data, date_col, interval, fill = list(), start_date = NULL, end_date = NULL) {
  date_col = ensym(date_col)

  dates.range <- range(dplyr::pull(data, !!date_col))

  if (is.null(start_date)) {
    start_date <- dates.range[1]
  }

  if (is.null(end_date)) {
    end_date <- dates.range[2]
  }

  start_date <- lubridate::floor_date(start_date, "year")
  end_date <- lubridate::ceiling_date(end_date, "year")

  # sequence includes the last and this is usually the first value of a new year
  # so we drop it if the date isn't included in data
  drop_last <- (dates.range[2] != end_date)
  dates <- seq(start_date, end_date, by = interval)

  if (isTRUE(drop_last)) {
    dates <- utils::head(dates, -1)
  }

  data <- tidyr::complete(data, !!date_col := dates, fill = fill)
  data
}



#' Pads data to complete year and fill up specific columns
#'
#' @description This function pad data to complete years. The fill mechanism is:
#' * find min- and max time
#' * group data by columns provided by `...` or all factor/character columns
#' * [dplyr::group_nest()]
#' * pass the nested data for each group to [pad_to_year()] with the min- and max time
#' * [tidyr::unnest()]
#'
#' @param data input data
#' @param date_col column containing time information
#' @param interval interval between two dates. See [seq.POSIXt()] for valid values
#' @param ... columns to fill up
#'
#' @return padded data
#' @export
#'
#' @examples
#' fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_min30_2013_Jan.csv")
#' data <- rOstluft::read_airmo_csv(fn)
#'
#' tail(data)
#'
#' # fills up all factor and character columns
#' pad_to_year_fill(data, starttime, "30 min") %>% tail()
#'
#' # or only the specific and the rest with na
#' pad_to_year_fill(data, starttime, "30 min", site) %>% tail()
#'
#' # data in wide format
#' data <- rOstluft::rolf_to_openair(data)
#' pad_to_year_fill(data, date, "30 min") %>% tail()
#'
#' # but be careful not to include cutting factors
#' data <- openair::cutData(data, "month") %>%
#'   dplyr::select(month, dplyr::everything())
#'
#' # fills up month and would multiply the data by the numbers of months
#' pad_to_year_fill(data, date, "30 min") %>% tail()
#'
#' # only fills up the site column
#' pad_to_year_fill(data, date, "30 min", site) %>% tail()
pad_to_year_fill <- function(data, date_col, interval, ...) {
  dots <- rlang::ensyms(...)

  if (length(dots) == 0) {
    dots <- which(sapply(data, function(x) is.factor(x) || is.character(x)))
    dots <- rlang::syms(names(dots))
  }

  col_order <- rlang::names2(data)
  date_col <- rlang::ensym(date_col)
  dates.range <- range(dplyr::pull(data, !!date_col))
  data <- dplyr::group_nest(data, !!!dots, .key = "serie")
  data$serie = purrr::map(data$serie, pad_to_year, !!date_col, interval = interval,
                          start_date = dates.range[1], end_date = dates.range[2])

  data <- tidyr::unnest(data, .data$serie)
  dplyr::select(data, !!!col_order)
}
