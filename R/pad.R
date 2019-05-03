#' Pads data to complete year
#'
#' Generic padding function.
#'
#' @param data input data
#' @param date_col column containing date information
#' @param interval interval between two dates
#'
#' @return padded data
#' @export
pad_to_year <- function(data, date_col, interval) {
  date_col = ensym(date_col)

  dates <- dplyr::pull(data, !!date_col)
  start_date <- lubridate::floor_date(min(dates), "year")
  end_max <- max(dates)
  end_date <- lubridate::ceiling_date(end_max, "year")
  drop_last <- (end_max != end_date)

  dates <- seq(start_date, end_date, by = interval)

  if (isTRUE(drop_last)) {
    dates <- utils::head(dates, -1)
  }

  dates <- tibble::tibble( !!date_col := dates)

  dplyr::right_join(data, dates, by = rlang::as_name(date_col))
}



#' Pads data to complete year and fill up specific columns
#'
#' @param data input data
#' @param date_col column containing date information
#' @param interval interval between two dates
#' @param ... columns to fill up
#'
#' @return padded data
#' @export
pad_to_year_fill <- function(data, date_col, interval, ...) {
  data <- dplyr::group_nest(data, ..., .key = "serie")
  data <- dplyr::mutate(data,
    serie = purrr::map(.data$serie, pad_to_year, date_col, interval)
  )
  tidyr::unnest(data, .data$serie)
}
