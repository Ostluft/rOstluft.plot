#' ggplot2-wrapper to summarise and plot data by wind direction bins as radar plot
#'
#' @param data tibble containing wind speed, wind direction and air pollutant concentration data
#' @param mapping ggplot2 mapping, e.g. aes(wd = wd, ws = ws, z = NOx); require aesthetics wd, ws, z
#' @param nmin numeric, minimum number of data points to be averaged in one wind direction bin
#' @param fun character string or vector of character strings, stat function(s) to be applied at wind direction bins
#' @param fun.args list, arguments to fun
#' @param wd_binwidth numeric, binwidth for wind direction in °, wd_binwidth should fullfill: (360 / wd_binwidth) %in% c(4, 8, 12, 16)
#' @param wd_cutfun function, cut function for wind direction (to create bins)
#' @param color_scale ggplot2 discrete color scale, e.g. scale_color_gradientn(...)
#' @param fill_scale ggplot2 discrete fill scale, e.g. scale_fill_gradientn(...)
#' @param geom character string for ggplot2 geom used in plot, here: "polygon"
#' @param bg raster map, e.g. ggmap object as plot background
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
#' ggradar(df, aes(wd = wd, ws = ws, z = NOx), color = "blue", fill = "blue") + ylab("NOx")
#'
#' q95 <- function(x, ...) quantile(x, 0.95, ...)
#' ggradar(df, aes(wd = wd, ws = ws, z = NOx, group = stat(stat), color = stat(stat)),
#'         fill = NA, fun = list("mean", "median", "perc95" = q95)) + ylab("NOx")
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
#'   theme(panel.grid.major = ggplot2::element_line(linetype = 1, color = "white"))
#'
#'
#' @export
ggradar <- function(data,
                    mapping,
                    nmin = 3,
                    fun = "mean",
                    fun.args = list(na.rm = TRUE),
                    wd_binwidth = 45,
                    wd_cutfun = cut_wd.fun(binwidth = 45),
                    color_scale = scale_color_viridis_d(),
                    fill_scale = scale_fill_viridis_d(alpha = 0.25),
                    geom = "polygon",
                    bg = NULL,
                    ...
) {

  breaks <- levels(wd_cutfun(seq(0, 360, wd_binwidth)))[seq(1, 360 / wd_binwidth, 90 / wd_binwidth)]
  plot <-
    ggplot(data, mapping) +
    stat_summary_wind(
      mapping = aes(x = stat(wd), y = stat(z)),
      fun = fun, fun.args = fun.args, nmin = nmin, geom = geom, wd_cutfun = wd_cutfun,
      wd_offset = wd_binwidth / 2, ws_cutfun = identity, groups = NULL, ...
    ) +
    coord_radar(start = -2 * pi / 360 * wd_binwidth / 2, bg = bg) +
    scale_x_discrete(breaks = breaks, labels = c("N", "E", "S", "W")) +
    scale_y_continuous(limits = c(0, NA), expand = c(0,0)) +
    color_scale +
    fill_scale +
    theme_radar

  return(plot)
}















