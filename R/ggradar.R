#' ggplot2-wrapper to summarise and plot data by wind direction bins as radar plot
#'
#' @param data tibble containing wind speed, wind direction and air pollutant concentration data
#' @param mapping parameter mapping for [ggplot2::ggplot()] using [ggplot2::aes()]; required mapping: x, y
#' (x has to be wind direction and y the parameter of interest); for more details, check out the examples
#' @param facet_groups symbol or string specifying the variable(s) for facetting; passed to [summary_wind()]
#' by using the [groups()] function
#' (facet_groups and groups are passed to [summary_wind()] groupings argument); default = groups()
#' @param fun character string or vector of character strings, stat function(s) to be applied at wind direction bins
#' @param fun.args list, arguments to fun
#' @param nmin numeric, minimum number of data points to be averaged in one wind direction bin
#' @param wd_binwidth numeric, binwidth for wind direction in °, wd_binwidth should fullfill:
#'   `(360 / wd_binwidth) %in% c(4, 8, 16, 32)`
#' @param fun_reorder a function (default is [identity()]) used to reorder the factor levels of the mapping's group variable
#' (can be useful to change the fill order, see examples)
#' @param bg raster map, e.g. ggmap object as plot background
#' @param ... other parameters passed on to [ggplot2::geom_polygon()]
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
#' data <-
#'   rOstluft::read_airmo_csv(fn) %>%
#'   rOstluft::rolf_to_openair() %>%
#'   dplyr::mutate(wday = lubridate::wday(date, label = TRUE, week_start = 1))
#'
#' # wind direction radar chart with mean values of y as summary statistics
#' ggradar(data, mapping = aes(x = wd, y = NOx))
#'
#' # wind direction radar chart with pre-calculated summary stats
#' data %>%
#'    filter(!is.na(wd)) %>%
#'    mutate(wd = cut_wd(wd, binwidth = 45)) %>%
#'    group_by(wd) %>%
#'    summarise(NOx = mean(NOx, na.rm = TRUE)) %>%
#'    ungroup() %>%
#'    ggradar(mapping = aes(x = wd, y = NOx), fun = "identity")
#'
#' # same as above but with defined fill and alpha, no color
#' ggradar(data, mapping = aes(x = wd, y = NOx), fill = "gray30", alpha = 0.5)
#'
#' # same as above but with no fill, defined color etc
#' ggradar(data, mapping = aes(x = wd, y = NOx), fill = NA, color = "steelblue", lwd = 1)
#'
#' # higher (actually: highest, then no axis labelling anymore) wind direction resolution
#' ggradar(data, mapping = aes(x = wd, y = NOx), wd_binwidth = 11.25, fun.args = list(na.rm = TRUE), nmin = 3, fill = "gray30", alpha = 0.5)
#'
#' # apply different statistic function
#' q95 <- function(x, ...) quantile(x, 0.95, ...)
#' ggradar(data, mapping = aes(x = wd, y = NOx), fun = "q95", alpha = 0.5)
#'
#' # group by multiple statistic functions and omit polygon filling
#' ggradar(data, mapping = aes(x = wd, y = NOx, color = stat, group = stat), fun = list("mean", "median", "perc95" = q95), fill = NA)
#'
#' # ... adjust x and color and fill scales and reorder stat levels for appropriate fill order
#' q05 <- function(x, ...) quantile(x, 0.05, ...)
#' q95 <- function(x, ...) quantile(x, 0.95, ...)
#' stat_reorder <- function(stat) factor(stat, levels = rev(c("perc05", "median", "mean", "perc95")))
#' ggradar(data, mapping = aes(x = wd, y = NOx, fill = stat, group = stat), fun = list("perc05" = q05, "median", "mean", "perc95" = q95),
#'    fun_reorder = stat_reorder, color = NA, alpha = 0.9) +
#'    scale_y_continuous(limits = c(0,120)) +
#'    scale_fill_viridis_d(begin = 0.2)
#'
#' # ... same as above but with one-colored fill and stats as facets
#' ggradar(data, mapping = aes(x = wd, y = NOx, group = stat), fun =  list("perc05" = q05, "median", "mean", "perc95" = q95), alpha = 0.6) +
#'   facet_wrap(stat~., ncol = 2)
#'
#' # multiple y-parameters and facetting (facetting variable has to be separately specified in facet_groups!)
#' data %>%
#'   dplyr::select(wd, PM10, NOx, wday) %>%
#'   tidyr::gather(par, val, -wd, -wday) %>%
#'   ggradar(mapping = aes(x = wd, y = val, group = wday, color = wday), facet_groups = groups(par), fill = NA) +
#'   facet_wrap(par~.)
#'
#' # with background map
#' bbox <- tibble::tibble(x = c(2683141 - 500, 2683141 + 500), y = c(1249040 - 500, 1249040 + 500))
#' bbox <- rOstluft::transform_LV95_to_WSG84(bbox)
#' bbox <- c(left = bbox$lon[1], right = bbox$lon[2], bottom = bbox$lat[1], top = bbox$lat[2])
#' raster_map <- ggmap::get_stamenmap(bbox, zoom = 16, maptype = "terrain",
#'                                    source = "stamen", color = "bw")
#'
#' ggradar(data, mapping = aes(x = wd, y = NOx), bg = raster_map, color = "steelblue", fill = NA, size = 1) +
#'   theme(panel.grid.major = ggplot2::element_line(linetype = 1, color = "white"))
#'
ggradar <- function(data,
                    mapping,
                    facet_groups = groups(),
                    wd_binwidth = 45,
                    fun = "mean",
                    fun.args = list(),
                    nmin = 3,
                    fun_reorder = identity,
                    bg = NULL,
                    ...
) {

  if ("identity" %in% fun) {
    data_summarized <- data
  } else {
    wd <- rlang::sym(rlang::as_name(mapping$x))
    wd_cutfun <- cut_wd.fun(binwidth = wd_binwidth)
    y <- rlang::sym(rlang::as_name(mapping$y))
    if (is.null(mapping$group)) {
      grp <- groups()
    } else if ("stat" %in% as.character(mapping$group)) {
      grp <- groups()
    } else {
      grp <- groups(!!as.character(mapping$group)[2])
    }
    grp_var <- rlang::sym(ifelse(length(as.character(grp)) == 0, "stat", as.character(grp)))
    data_summarized <- summary_wind(data, NULL, !!wd, !!y, groupings = modify_list(grp, facet_groups),
                                    wd_cutfun = wd_cutfun, fun = fun, fun.args = fun.args)
    data_summarized <- dplyr::mutate(data_summarized, !!grp_var := fun_reorder(!!grp_var))
  }

  if (!("group" %in% names(mapping))) mapping <- modify_list(mapping, aes(group = NA))
  polygon_layer <- rlang::exec(geom_polygon, ...)

  plot <-
    ggplot(data_summarized, mapping) +
    polygon_layer +
    coord_radar(start = -2 * pi / 360 * wd_binwidth / 2, bg = bg) +
    scale_x_discrete() +
    scale_y_continuous(limits = c(0, NA), expand = c(0,0)) +
    ylab(y) +
    theme_radar

  return(plot)
}















