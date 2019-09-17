#' ggplot2-wrapper to summarise and plot data by wind direction bins as radar plot
#'
#' @param data tibble containing wind speed, wind direction and air pollutant concentration data
#' @param group symbol or string specifying the group argument for (internal) mapping;
#' default = NULL (=> internally set to "stat" to match the outcome of [summary_wind()])
#' @param facet_groups symbol or string specifying the variable(s) for facetting
#' (facet_groups and groups are passed to [summary_wind()] groupings argument); default = NULL
#' @param fun character string or vector of character strings, stat function(s) to be applied at wind direction bins
#' @param fun.args list, arguments to fun
#' @param nmin numeric, minimum number of data points to be averaged in one wind direction bin
#' @param wd_binwidth numeric, binwidth for wind direction in °, wd_binwidth should fullfill:
#'   `(360 / wd_binwidth) %in% c(4, 8, 16, 32)`
#' @param color_scale ggplot2 discrete color scale, e.g. [ggplot2::scale_color_gradientn()]; can be NA (then, no color is plotted)
#' @param fill_scale ggplot2 discrete fill scale, e.g. [ggplot2::scale_fill_gradientn()]; can be NA (then, no fill is plotted)
#' @param bg raster map, e.g. ggmap object as plot background
#' @inheritParams summary_wind
#'
#' @return [ggplot2::ggplot()] object
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(tidyr)
#'
#' fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_2010-2014.csv")
#'
#' df <-
#'   rOstluft::read_airmo_csv(fn) %>%
#'   rOstluft::rolf_to_openair() %>%
#'   dplyr::mutate(wday = lubridate::wday(date, label = TRUE, week_start = 1))
#'
#' # wind direction radar chart with mean values of y as summary statistics
#' ggradar(df, wd = wd, y = NOx)
#'
#' # same as above but with fixed fill and definde alpha and no color (must be set via _scale argument)
#' ggradar(df, wd = wd, y = NOx, color_scale = NA, fill_scale = scale_fill_manual(values = alpha("gray60", 0.5)))
#'
#' # apply different statistic function
#' q95 <- function(x, ...) quantile(x, 0.95, ...)
#' ggradar(df, wd = wd, y = NOx, fun = "q95")
#'
#' # group by multiple statistic functions and omit polygon filling
#' ggradar(df, wd = wd, y = NOx, fun = list("mean", "median", "perc95" = q95), fill_scale = NA)
#'
#' # ... same as above but with one-colored fill and stats as facets
#' ggradar(df, wd = wd, y = NOx, fun = list("mean", "median", "perc95" = q95),
#'   color_scale = NA, fill_scale = scale_fill_manual(values = alpha(rep("gray40",3), 0.5))) +
#'   facet_wrap(stat~.)
#'
#' # multiple y-parameters and facetting (facetting variable has to be separately specified in facet_groups!)
#' df %>%
#'   dplyr::select(wd, NO, NOx, wday) %>%
#'   tidyr::gather(par, val, -wd, -wday) %>%
#'   ggradar(wd = wd, y = val, group = par, facet_groups = wday)) +
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
#' ggradar(df, wd = wd, y = NOx, bg = raster_map) +
#'   theme(panel.grid.major = ggplot2::element_line(linetype = 1, color = "white"))
#'
ggradar <- function(data, wd, y,
                    group = NULL,
                    facet_groups = NULL,
                    wd_binwidth = 45,
                    fun = "mean",
                    fun.args = list(),
                    nmin = 3,
                    reverse = TRUE,
                    color_scale = scale_color_viridis_d(),
                    fill_scale = scale_fill_viridis_d(alpha = 0.25),
                    bg = NULL,
                    ...
) {

  wd <- rlang::ensym(wd)
  wd_cutfun <- cut_wd.fun(binwidth = wd_binwidth)
  y <- rlang::ensym(y)
  groupings <- groups(group, facet_groups)

  data_summarized <- summary_wind(data, NULL, !!wd, !!y, groupings = groupings,
                                  wd_cutfun = wd_cutfun, fun = fun, fun.args = fun.args)

  if (is.null(group)) {
    group <- rlang::ensym("stat")
    theme_lgnd_gr <- "none"
  } else {
    group <- rlang::ensym(group)
    theme_lgnd_gr <- "right"
  }
  color <- group
  fill <- group

  if (is.na(fill_scale)) {
    polygon_layer <- rlang::exec(geom_polygon, !!!rlang::dots_list(...), fill =  NA)
    fill_scale <- NULL
  } else if (is.na(color_scale)) {
    polygon_layer <- rlang::exec(geom_polygon, !!!rlang::dots_list(...), color =  NA)
    color_scale <- NULL
  } else {
    polygon_layer <- rlang::exec(geom_polygon, !!!rlang::dots_list(...))
  }

  mapping <- modify_list(aes(x = !!wd, y = !!y, group = !!group), aes(color = !!color, fill = !!fill))

  plot <-
    ggplot(data_summarized, mapping) +
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















