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
#' # library(dplyr)
#' # library(tidyr)
#'
#' fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_2010-2014.csv")
#'
#' df <-
#'   rOstluft::read_airmo_csv(fn) %>%
#'   rOstluft::rolf_to_openair() %>%
#'   dplyr::mutate(wday = lubridate::wday(date, label = TRUE, week_start = 1))
#'
#' # wind direction radar chart with mean values of y as summary statistics
#' ggradar(df, mapping = aes(x = wd, y = NOx))
#'
#' # same as above but with fixed fill, defined alpha and no color
#' ggradar(df, mapping = aes(x = wd, y = NOx), fill = "gray30", alpha = 0.5)
#'
#' # same as above but with no fill, defined color
#' ggradar(df, mapping = aes(x = wd, y = NOx), fill = NA, color = "gray30")
#'
#' # apply different statistic function
#' q95 <- function(x, ...) quantile(x, 0.95, ...)
#' ggradar(df, mapping = aes(x = wd, y = NOx), fun = "q95", alpha = 0.5)
#'
#' # group by multiple statistic functions and omit polygon filling
#' ggradar(df, mapping = aes(x = wd, y = NOx, color = stat, group = stat),
#'         fun = list("mean", "median", "perc95" = q95), fill = NA)
#'
#' # ... same as above but with one-colored fill and stats as facets
#' ggradar(df, mapping = aes(x = wd, y = NOx, group = stat),
#'         fun = list("mean", "median", "perc95" = q95)) +
#'   facet_wrap(stat~.)
#'
#' # multiple y-parameters and facetting (facetting variable has to be separately specified in facet_groups!)
#' df2 <- dplyr::select(df, wd, NO, NOx, wday) %>%
#'   tidyr::gather(par, val, -wd, -wday)
#'
#' ggradar(df2, mapping = aes(x = wd, y = val, group = wday, color = wday),
#'         facet_groups = grp(par), fill = NA) +
#'   facet_wrap(vars(par))
#'
#' # with background map
#' bb <- bbox_lv95(2683141, 1249040, 500)
#' bg <- get_stamen_map(bb)
#' ggradar(df, mapping = aes(x = wd, y = NOx), bg = bg, color = "blue", fill = NA) +
#'   theme(panel.grid.major = ggplot2::element_line(linetype = 1, color = "white"))
#'
ggradar <- function(data,
                    mapping,
                    facet_groups = grp(),
                    wd_binwidth = 45,
                    fun = "mean",
                    fun.args = list(),
                    nmin = 3,
                    reverse = TRUE,
                    bg = NULL,
                    ...
) {

  wd <- rlang::sym(rlang::as_name(mapping$x))
  wd_cutfun <- cut_wd.fun(binwidth = wd_binwidth)
  y <- rlang::sym(rlang::as_name(mapping$y))

  if (is.null(mapping$group)) {
    grp <- grp()
  } else if (rlang::as_name(mapping$group) == "stat") {
    grp <- grp()
  } else {
    grp <- grp(!!rlang::as_name(mapping$group))
  }
  grp <- modify_list(grp, facet_groups)

  data_summarized <- summary_wind(data, NULL, !!wd, !!y, groupings = grp,
                                  wd_cutfun = wd_cutfun, fun = fun, fun.args = fun.args)

  if (!("group" %in% names(mapping))) {
    mapping <- modify_list(mapping, aes(group = NA))
  }

  plot <-
    ggplot(data_summarized, mapping) +
    geom_polygon(...) +
    coord_radar(start = -2 * pi / 360 * wd_binwidth / 2, bg = bg) +
    scale_x_discrete() +
    scale_y_continuous(limits = c(0, NA), expand = c(0,0)) +
    ylab(y) +
    theme_rop_radar()

  return(plot)
}















