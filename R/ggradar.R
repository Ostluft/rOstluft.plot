#' Summarise and ggplot data by wind direction bins
#'
#' @param data tibble containing wind speed, wind direction and air pollutant concentration data
#' @param mapping ggplot2 mapping, e.g. aes(wd = wd, ws = ws, z = NOx); requires wd, ws, z
#' @param nmin numeric, minimum number of data points to be averaged in one wind direction bin
#' @param fun character string, stat function to be applied at wind direction bins
#' @param fun.args list, arguments to fun
#' @param ws_max maximum wind speed cap; last wind speed bin contains all wind speeds > ws_max
#' @param wd_binwidth numeric, binwidth for wind direction, typically %in% c(45, 22.5)
#' @param color_scale ggplot2 color scale, e.g. scale_color_gradientn(...)
#' @param fill_scale ggplot2 fill scale, e.g. scale_fill_gradientn(...)
#' @param bg raster map, e.g. ggmap object as plot background
#' @param wd_cutfun NULL or a function with which wind direction is cut into bins; per default (wd_cutfun == NULL): function(wd) wd_classes(wd, wd_binwidth = wd_binwidth)
#'
#'
#' @examples
#' require(rOstluft)
#' require(rOstluft.data)
#' require(rOstluft.plot)
#' require(lubridate)
#' require(ggplot2)
#'
#' df <-
#'   rOstluft::read_airmo_csv(system.file("extdata", "Zch_Stampfenbachstrasse_2010-2014.csv",package = "rOstluft.data", mustWork = TRUE)) %>%
#'   rOstluft::rolf_to_openair() %>%
#'   dplyr::mutate(wday = lubridate::wday(date, label = TRUE, week_start = 1))
#'
#' ggradar(df, aes(wd = wd, ws = ws, z = NOx), fill = "blue", color = "blue", alpha = 0.5) + ylab("NOx")
#'
#' q95 <- function(x, ...) quantile(x, 0.95, ...)
#' ggradar(df, aes(wd = wd, ws = ws, z = NOx, group = stat(stat), color = stat(stat), fill = stat(stat)), fill = NA, fun = list(a = "mean", "median", "q95")) + ylab("NOx")
#'
#' df %>%
#'   dplyr::select(wd, ws, NO, NOx, wday) %>%
#'   tidyr::gather(par, val, -wd, -ws, -wday) %>%
#'   ggradar(aes(wd = wd, ws = ws, z = val, group = par, fill = par, color = par)) + ylab("mean") +
#'   facet_wrap(wday~.)
#'
#' # with background map
#' bbox <- tibble::tibble(x = c(2683141 - 500, 2683141 + 500), y = c(1249040 - 500, 1249040 + 500))
#' bbox <- rOstluft::transform_LV95_to_WSG84(bbox)
#' bbox <- c(left = bbox$lon[1], right = bbox$lon[2], bottom = bbox$lat[1], top = bbox$lat[2])
#'
#' raster_map <- ggmap::get_stamenmap(bbox, zoom = 16, maptype = "terrain",
#'                                    source = "stamen", color = "bw")
#'
#' ggradar(df, aes(wd = wd, ws = ws, z = NOx), fill = "blue", color = "blue", alpha = 0.2, bg = raster_map) +
#'   ylab("NOx") +
#'   theme( panel.grid.major = ggplot2::element_line(linetype = 1, color = "white"))
#'
#' @export
ggradar <- function(data,
                    mapping,
                    ...,
                    nmin = 3,
                    fun = "mean",
                    fun.args = list(na.rm = TRUE),
                    ws_max = NA,
                    wd_binwidth = 45,
                    color_scale = viridis::scale_color_viridis(discrete = TRUE),
                    fill_scale = viridis::scale_fill_viridis(discrete = TRUE, alpha = 0.25),
                    geom = "polygon",
                    bg = NULL,
                    wd_cutfun = NULL
) {

  if (is.null(wd_cutfun)) wd_cutfun <- function(wd) wd_classes(wd, wd_binwidth = wd_binwidth)
  breaks <- levels(wd_cutfun(seq(0, 360, wd_binwidth)))[seq(1, 360 / wd_binwidth, 90 / wd_binwidth)]

  plot <-
    ggplot(data, mapping) +
    stat_summary_wind(
      mapping = aes(x = stat(wd), y = stat(z)),
      ...,
      fun = fun, fun.args = fun.args, nmin = nmin, ws_max = ws_max, geom = geom, wd_cutfun = wd_cutfun,
      wd_offset = wd_binwidth / 2, ws_cutfun = ws_classes, groups = NULL
    ) +
    coord_radar(start = -2 * pi / 360 * wd_binwidth / 2, bg = bg) +
    scale_x_discrete(breaks = breaks, labels = c("N", "E", "S", "W")) +
    scale_y_continuous(limits = c(0, NA), expand = c(0,0)) +
    color_scale +
    fill_scale +
    theme_radar

  return(plot)
}















