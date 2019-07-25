#' ggplot wrapper to create a windrose (polar wind-bin frequency plot)
#'
#' @return ggplot object
#'
#' @param data tibble containing wind speed, wind direction and/or air pollutant concentration data
#' @param mapping ggplot2 mapping, e.g. aes(wd = wd, ws = ws); requires wd, ws
#' @param wd_binwidth numeric, binwidth for wind direction in °, wd_binwidth should fullfill:
#'   `(360 / wd_binwidth) %in% c(4, 8, 12, 16)`
#' @param ws_binwidth numeric, binwidth for wind speed
#' @param ws_max numeric, can be NA, wind speed is squished at this value
#' @param fill_scale ggplot2 discrete fill scale, e.g. [ggplot2::scale_fill_gradientn()]
#' @param reverse TRUE/FALSE, should wind speed bin factors be sorted descending or ascending (inside-out or reverse)?
#' @param bg raster map, e.g. ggmap object as plot background
#' @param param_args named list, passed on to [ggplot2::layer()] as argument params after combining with the other
#'   arguments
#' @param ... passed onto [stat_summary_wind()]
#'
#' @return [ggplot2::ggplot()] object
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' df <-
#'   rOstluft::read_airmo_csv(system.file("extdata", "Zch_Stampfenbachstrasse_2010-2014.csv", package = "rOstluft.data", mustWork = TRUE)) %>%
#'   rOstluft::rolf_to_openair() %>%
#'   openair::cutData(date, type = "daylight")
#'
#' ggwindrose(df, aes(ws = ws, wd = wd), wd_binwidth = 22.5,
#'            ws_binwidth = 0.5, ws_max = 5)
#'
#' # don't like bar outlines?
#' ggwindrose(df, aes(ws = ws, wd = wd), wd_binwidth = 22.5, param_args = list(color = "black"),
#'            ws_binwidth = 0.5, ws_max = 4)
#'
#' ggwindrose(df, aes(ws = ws, wd = wd), wd_binwidth = 22.5, param_args = list(color = NA),
#'            ws_binwidth = 0.5, ws_max = 4)
#'
#' # facetting
#' ggwindrose(df, aes(ws = ws, wd = wd), wd_binwidth = 22.5, param_args = list(color = NA),
#'             ws_binwidth = 0.5, ws_max = 3) +
#'   facet_wrap(daylight~.)
ggwindrose <- function(data,
                       mapping,
                       param_args = list(),
                       ...,
                       wd_binwidth = 45,
                       ws_binwidth = 1,
                       ws_max = NA,
                       fill_scale = scale_fill_viridis_d(),
                       reverse = TRUE,
                       bg = NULL
) {

  wd_cutfun <- cut_wd.fun(binwidth = wd_binwidth)
  ws_cutfun <- cut_ws.fun(binwidth = ws_binwidth, ws_max = ws_max, reverse = reverse)
  param_args <- modify_list(list(color = "white", width = 1, size = 0.25), param_args)
  mapping$z <- mapping$ws

  plot <-
    ggplot(data, mapping) +
    stat_summary_wind(
      mapping = aes(x = as.numeric(stat(wd)), y = stat(freq), group = stat(ws), fill = stat(ws)),
      layer_args = list(geom = "bar"),
      param_args = param_args,
      wd_cutfun = wd_cutfun,
      wd_offset = wd_binwidth / 2, ws_cutfun = ws_cutfun, groups = c("wd", "ws"),
      ...
    ) +
    coord_polar2(start = -2 * pi / 360 * wd_binwidth / 2, bg = bg) +
    scale_x_continuous(breaks = c(0, 90, 180, 270) / wd_binwidth + 1, labels = c("N", "E", "S", "W"), expand = c(0,0)) +
    scale_y_continuous(limits = c(0, NA), expand = c(0,0), labels = scales::percent) +
    fill_scale +
    guides(fill = guide_legend(title = rlang::quo_text(mapping$z))) +
    theme_windrose

  return(plot)
}
