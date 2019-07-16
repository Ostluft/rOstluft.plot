#' ggplot heatmap for diurnal-yearly time course
#'
#' @description Create a heatmap with date on x-axis and time of day on y-axis; z values as colour scale.
#'
#' @param data a data.frame or tibble with input data (containing a POSIXct variable as time parameter).
#' @param time character string giving time variable name.
#' @param z character string giving z value variable name.
#' @param etc ...
#' @param ... Other arguments passed on to geom_raster().
#'
#' @return ggplot
#'
#' @examples
#' require(rOstluft)
#' require(rOstluft.data)
#' require(rOstluft.plot)
#' require(ggplot2)
#' require(dplyr)
#'
#' df <-
#'   rOstluft::read_airmo_csv(system.file("extdata", "Zch_Stampfenbachstrasse_2010-2014.csv", package = "rOstluft.data", mustWork = TRUE)) %>%
#'   rOstluft::rolf_to_openair() %>%
#'   mutate(year = lubridate::year(date))
#'
#' ggyearday(df, time = "date", z = "O3") +
#'   facet_wrap(year~., scales = "free_x", ncol = 1)
#'
#' # data with outliers / extreme values
#' ggyearday(df, time = "date", z = "PM10", fill_scale = fill_scale) +
#'   facet_wrap(year~., scales = "free_x", ncol = 1)
#'
#'
#' # use a custom scale and squish the outliers / extreme values
#' fill_scale <- scale_fill_viridis_c(breaks=c(0, 25, 50, 75), limits = c(0, 75), oob = scales::squish,
#'                                    direction = -1, na.value = NA, option = "A")
#' ggyearday(df, time = "date", z = "PM10", fill_scale = fill_scale) +
#'   facet_wrap(year~., scales = "free_x", ncol = 1)
#'
#' @export
ggyearday <- function(data, time, z, xbreaks = "1 month", xlabels = "%b", ybreaks = seq(3,21,3), ylabels = waiver(),
                      fill_scale = scale_fill_viridis_c(direction = -1, na.value = NA, option = "A"), ...) {

  if (class(fill_scale$labels) == "waiver" & class(fill_scale$breaks) != "waiver") {
    fill_scale$labels <- c(head(fill_scale$breaks, -1), paste0(">",tail(fill_scale$breaks, 1)))
  }

  mapping <- aes(
    x = lubridate::as_date(!!rlang::sym(time)),
    y = lubridate::hour(!!rlang::sym(time)),
    fill = !!rlang::sym(z)
  )

  ggplot(data, mapping) +
    geom_raster(...) +
    fill_scale +
    scale_x_date(date_breaks = xbreaks, date_labels = xlabels, expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0), breaks = ybreaks, labels = ylabels,  position = "right") +
    theme_bw() +
    theme(
      axis.title = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      strip.background = element_blank(),
      strip.text = element_text(hjust = 0, size = 10)
    )
}
