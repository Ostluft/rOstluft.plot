#' ggplot wrapper to create a windrose (polar wind-bin frequency plot)
#'
#' @return ggplot object
#'
#' @param data tibble containing wind speed, wind direction and/or air pollutant concentration data
#' @param mapping ggplot2 mapping, e.g. aes(wd = wd, ws = ws); requires wd, ws
#' @param wd_binwidth numeric, binwidth for wind direction, typically %in% c(45, 22.5)
#' @param ws_max maximum wind speed cap; last wind speed bin contains all wind speeds > ws_max
#' @param fill_scale ggplot2 fill scale, e.g. scale_fill_gradientn(...)
#' @param bg raster map, e.g. ggmap object as plot background
#' @param wd_cutfun NULL or a function with which wind direction is cut into bins; per default (wd_cutfun == NULL): function(wd) wd_classes(wd, wd_binwidth = wd_binwidth)
#' @param ws_cutfun NULL or a function with which wind speed is cut into bins; per default (ws_cutfun == NULL): function(ws) ws_classes(ws, ws_max = ws_max)
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
#' ggwindrose(df, aes(ws = ws, wd = wd), wd_binwidth = 22.5, ws_cutfun = cut_ws.fun(ws_binwidth = 0.5, ws_max = 3))
#'
#' ggwindrose(df, aes(ws = ws, wd = wd), wd_binwidth = 22.5, ws_cutfun = cut_ws.fun(ws_binwidth = 0.5, ws_max = 3)) +
#'   facet_wrap(daylight~.)
#'
#' @export
ggwindrose <- function(data,
                       mapping,
                       ...,
                       wd_binwidth = 45,
                       ws_max = NA,
                       fill_scale = scale_fill_viridis_d(),
                       bg = NULL,
                       wd_cutfun = NULL,
                       ws_cutfun = NULL
) {

#xxx wieso mapping als argument? macht nur sinn wenn wir mehr flexibilät brauchen würden
#für einen vordefinierten wrapper, bei dem man nur ws und wd als input hat denk ich einfach für den user
#die spalte als symbol zu übergeben
#möglichkeit für frequency statt n?

  if (is.null(wd_cutfun)) wd_cutfun <- cut_wd.fun(wd_binwidth = wd_binwidth)
  if (is.null(ws_cutfun)) ws_cutfun <- cut_ws.fun(ws_max = ws_max)
  mapping$z <- mapping$ws
  # breaks <- seq(0, 360, wd_binwidth)
  # breaks <- paste0("[", head(breaks, -1),"," ,tail(breaks, -1), ")")[seq(1, 360 / wd_binwidth, 90 / wd_binwidth)]

  plot <-
    ggplot(data, mapping) +
    stat_summary_wind(
      mapping = aes(x = as.numeric(stat(wd)), y = stat(n), group = stat(ws), fill = stat(ws)),
      ...,
      geom = "bar", wd_cutfun = wd_cutfun,
      wd_offset = wd_binwidth / 2, ws_cutfun = ws_cutfun, groups = c("wd", "ws"),
      position = "stack", color = "white", width = 1, size = 0.25
    ) +
    coord_polar2(start = -2 * pi / 360 * wd_binwidth / 2, bg = bg) +
    scale_x_continuous(breaks = c(0, 90, 180, 270) / wd_binwidth + 1, labels = c("N", "E", "S", "W"), expand = c(0,0)) +
    scale_y_continuous(limits = c(0, NA), expand = c(0,0)) +
    fill_scale +
    guides(fill = guide_legend(title = rlang::quo_text(mapping$z))) +
    theme_windrose

  return(plot)
}
