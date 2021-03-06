#' ggplot2 heatmap for diurnal-yearly time series
#'
#' @description creates a heatmap with date on x-axis and time of day on y-axis; z values as fill scale.
#'
#' @param data a data.frame or tibble with input data (containing a POSIXct variable as time parameter).
#' @param time symbol giving time column
#' @param z symbol giving z column used as fill
#' @param date_breaks character string as input for [ggplot2::scale_x_date()],
#'   e.g. '1 month', defines date breaks on x-axis.
#' @param date_labels character string as input for [ggplot2::scale_x_date()],
#'   formatter for date labels on x-axis.
#' @param ybreaks numeric vector, specifies y-axis breaks.
#' @param ylabels function, format function for y-axis labels.
#' @param fill_scale ggplot2 continuous fill scale, e.g. [scale_fill_gradient()].
#' @param ... other arguments passed on to [ggplot2::geom_raster()].
#'
#' @return ggplot
#'
#' @examples
#' library(ggplot2)
#'
#' fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_2010-2014.csv")
#'
#' # only 4 years for smaller plot size in examples
#' df <-
#'   rOstluft::read_airmo_csv(fn) %>%
#'   dplyr::filter(starttime < lubridate::ymd(20140101)) %>%
#'   rOstluft::rolf_to_openair()
#'
#' ggyearday(df, time = "date", z = "O3")
#'
#' # data with outliers / extreme values => not very informative...
#' ggyearday(df, time = date, z = PM10)
#'
#' # ...use a custom scale and squish the outliers / extreme values
#' fill_scale <- scale_fill_viridis_squished(breaks=c(0, 25, 50, 75), limits = c(0, 75),
#'                                           direction = -1, na.value = NA, option = "A")
#'
#' ggyearday(df, time = date, z = PM10, fill_scale = fill_scale)
#'
#' @export
ggyearday <- function(data, time, z, date_breaks = "1 month", date_labels = "%b",
                      ybreaks = seq(6,18,6), ylabels = format_sprintf("%02d:00"),
                      fill_scale = scale_fill_viridis_c(direction = -1, na.value = NA, option = "A"),
                      ...) {

  time <- rlang::ensym(time)
  z <- rlang::ensym(z)
  data <- dplyr::mutate(data, year = lubridate::year(!!time))

  mapping <- aes(
    x = lubridate::as_date(!!time),
    y = lubridate::hour(!!time),
    fill = !!z
  )

  ggplot(data, mapping) +
    geom_raster(...) +
    fill_scale +
    scale_x_date(date_breaks = date_breaks, date_labels = date_labels, expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0), breaks = ybreaks, labels = ylabels,  position = "right") +
    facet_wrap(vars(.data$year), scales = "free_x", ncol = 1) +
    theme_bw() +
    theme(
      axis.title = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      strip.background = element_blank(),
      strip.text = element_text(hjust = 0, size = 10)
    )
}

