#' ggplot2-wrapper to summarise and plot data by wind direction bins as radar plot
#'
#' @param data tibble containing wind speed, wind direction and air pollutant concentration data
#' @param fun character string or vector of character strings, stat function(s) to be applied at wind direction bins
#' @param fun.args list, arguments to fun
#' @param nmin numeric, minimum number of data points to be averaged in one wind direction bin
#' @param wd_binwidth numeric, binwidth for wind direction in °, wd_binwidth should fullfill:
#'   `(360 / wd_binwidth) %in% c(4, 8, 16, 32)`
#' @param color_scale ggplot2 discrete color scale, e.g. [ggplot2::scale_color_gradientn()]
#' @param fill_scale ggplot2 discrete fill scale, e.g. [ggplot2::scale_fill_gradientn()]
#' @param bg raster map, e.g. ggmap object as plot background
#' @inheritParams summary_wind
#'
#' @return [ggplot2::ggplot()] object
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_2010-2014.csv")
#'
#' df <-
#'   rOstluft::read_airmo_csv(fn) %>%
#'   rOstluft::rolf_to_openair() %>%
#'   dplyr::mutate(wday = lubridate::wday(date, label = TRUE, week_start = 1))
#'
#' ggradar(df, aes(wd = wd, ws = ws, z = NOx),
#'         param_args = list(fill = "blue", color = "blue", alpha = 0.5)) +
#'   ylab("NOx")
#'
#' q95 <- function(x, ...) quantile(x, 0.95, ...)
#' ggradar(df, aes(wd = wd, ws = ws, z = NOx, group = stat(stat), color = stat(stat)),
#'         param_args = list(fill = NA), fun = list("mean", "median", "perc95" = q95)) + ylab("NOx")
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
#' ggradar(df, aes(wd = wd, ws = ws, z = NOx), bg = raster_map,
#'         param_args = list(fill = "blue", color = "blue", alpha = 0.2)) +
#'   ylab("NOx") +
#'   theme(panel.grid.major = ggplot2::element_line(linetype = 1, color = "white"))
ggradar <- function(data, wd, y,
                    groupings = groups(),
                    wd_binwidth = 45,
                    fun = "mean",
                    fun.args = list(na.rm = TRUE),
                    nmin = 3,
                    reverse = TRUE,
                    color_scale = scale_color_viridis_d(),
                    fill_scale = scale_fill_viridis_d(alpha = 0.25),
                    bg = NULL,
                    ...
) {

  wd <- rlang::ensym(wd) # or enquo but summary_wind accept only strings or symbols
  wd_cutfun <- cut_wd.fun(binwidth = wd_binwidth)
  ws_cutfun <- function(ws) 1
  ws <- rlang::ensym(y)
  y2 <- rlang::ensym(paste0(y,".stat"))
  data_summarized <- summary_wind(data, !!ws, !!wd, !!ws, groupings = groupings,
                                  wd_cutfun = wd_cutfun, ws_cutfun = ws_cutfun, fun = fun, fun.args = fun.args)
  data_summarized <- dplyr::select(data_summarized, -!!ws)
  if (length(groupings) == 0) {
    groupings <- "dummy"
    fill <- paste0(y,".stat")
    color <- paste0(y,".stat")
    theme_lgnd_gr <- "none"
  } else {
    groupings <- rlang::ensym(as.character(groupings[[1]])[2])
    fill <- groupings
    color <- groupings
    theme_lgnd_gr <- "right"
  }
  polygon_layer <- rlang::exec(geom_polygon, !!!rlang::dots_list())

  plot <-
    ggplot(data_summarized, aes(x = !!wd, y = !!y2, color = !!color, fill = !!fill, group = !!groupings)) +
    polygon_layer +
    coord_radar(start = -2 * pi / 360 * wd_binwidth / 2, bg = bg) +
    scale_x_discrete() +
    scale_y_continuous(limits = c(0, NA), expand = c(0,0)) +
    color_scale +
    fill_scale +
    ylab(y) +
    theme_radar +
    theme(legend.position = theme_lgnd_gr)

  return(plot)
}















