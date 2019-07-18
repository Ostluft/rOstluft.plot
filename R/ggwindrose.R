#' ggplot wrapper to create a windrose (polar wind-bin frequency plot)
#'
#' @return ggplot object
#'
#' @param data tibble containing wind speed, wind direction and/or air pollutant concentration data
#' @param mapping ggplot2 mapping, e.g. aes(wd = wd, ws = ws); requires wd, ws
#' @param wd_binwidth numeric, binwidth for wind direction in °, wd_binwidth should fullfill: (360 / wd_binwidth) %in% c(4, 8, 12, 16)
#' @param wd_cutfun function, cut function for wind direction (to create bins)
#' @param ws_cutfun function, cut function for wind speed
#' @param fill_scale ggplot2 discrete fill scale, e.g. scale_fill_gradientn(...)
#' @param bg raster map, e.g. ggmap object as plot background
#'
#'
#' @examples
#' require(rOstluft)
#' require(rOstluft.data)
#' require(rOstluft.plot)
#' require(ggplot2)
#' require(dplyr)
#' require(openair)
#'
#' df <-
#'   rOstluft::read_airmo_csv(system.file("extdata", "Zch_Stampfenbachstrasse_2010-2014.csv", package = "rOstluft.data", mustWork = TRUE)) %>%
#'   rOstluft::rolf_to_openair() %>%
#'   openair::cutData(date, type = "daylight")
#'
#' ggwindrose(df, aes(ws = ws, wd = wd), wd_binwidth = 22.5, wd_cutfun = cut_wd.fun(binwidth = 22.5), ws_cutfun = cut_ws.fun(binwidth = 1, ws_max = 5))
#'
#' # don't like bar outlines?
#' ggwindrose(df, aes(ws = ws, wd = wd), color = NA, wd_binwidth = 22.5, wd_cutfun = cut_wd.fun(binwidth = 22.5), ws_cutfun = cut_ws.fun(binwidth = 0.5, ws_max = 4))
#'
#' # facetting
#' ggwindrose(df, aes(ws = ws, wd = wd), color = NA, wd_binwidth = 22.5, wd_cutfun = cut_wd.fun(binwidth = 22.5), ws_cutfun = cut_ws.fun(binwidth = 0.5, ws_max = 3)) +
#'   facet_wrap(daylight~.)
#'
#' @export
ggwindrose <- function(data,
                       mapping,
                       ...,
                       wd_binwidth = 45,
                       wd_cutfun = cut_wd.fun(binwidth = 45),
                       ws_cutfun = cut_ws.fun(binwidth = 1, ws_max = NA),
                       fill_scale = scale_fill_viridis_d(direction = -1),
                       bg = NULL
) {

  mapping$z <- mapping$ws
  plot <-
    ggplot(data, mapping) +
    stat_summary_wind(
      mapping = aes(x = as.numeric(stat(wd)), y = stat(freq), group = stat(ws), fill = stat(ws)),
      ...,
      geom = "bar", wd_cutfun = wd_cutfun,
      wd_offset = wd_binwidth / 2, ws_cutfun = ws_cutfun, groups = c("wd", "ws"),
      color = "white", width = 1, size = 0.25
    ) +
    coord_polar2(start = -2 * pi / 360 * wd_binwidth / 2, bg = bg) +
    scale_x_continuous(breaks = c(0, 90, 180, 270) / wd_binwidth + 1, labels = c("N", "E", "S", "W"), expand = c(0,0)) +
    scale_y_continuous(limits = c(0, NA), expand = c(0,0), labels = scales::percent) +
    fill_scale +
    guides(fill = guide_legend(title = rlang::quo_text(mapping$z))) +
    theme_windrose

  return(plot)
}
