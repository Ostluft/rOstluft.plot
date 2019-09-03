#' Summarise z values over binned wind data.
#'
#' @description All calculation are done by [summary_wind()], see the documentation for the details.
#' [stat_summary_wind()] uses the aesthetics `ws`, `wd` and `z` to map the correspondent arguments
#' for [summary_wind()]. The computed variables are named `ws`, `wd` and `z`. [geom_bar_wind()]
#' is an extension to [ggplot2::geom_bar()] with [stat_summary_wind()] as default stat and correct
#' handling of the factorized `x` aesthetic. To map the computed Variables use [ggplot2::stat()].
#'
#' @section Grouping:
#'
#'
#' @param ... Other arguments passed on to [layer()]. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   `colour = "red"` or `size = 3`. They may also be parameters
#'   to the paired geom/stat.
#'
#' @inheritParams summary_wind
#' @inheritParams ggplot2::layer
#'
#' @return [ggplot2::layer()]
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_2010-2014.csv")
#' data <- rOstluft::read_airmo_csv(fn)
#' data <- rOstluft::rolf_to_openair(data)
#'
#' # a simple wind rose
#' ggplot(data, aes(x = stat(wd), y = stat(freq), fill = stat(ws))) +
#'   geom_bar_wind(
#'     mapping = aes(wd = wd, ws = ws, z = ws),
#'     ws_cutfun = cut_ws.fun(ws_max = 4),
#'     width = 1,
#'     color = "white"
#'   ) +
#'   coord_polar2(start = - 22.5 / 180 * pi ) +
#'   scale_fill_viridis_d(direction = -1)
#'
#' #
#'
#' # use stat_summary_wind to generate a radar plot using
#' # the polygon geom, position "identity" and coord_radar()
#' f <- list(
#'   "median",
#'   "mean",
#'   "perc95" = ~ stats::quantile(., probs = 0.95)
#' )
#'
#' ggplot(data, aes(x = stat(wd), y = stat(z), color = stat(stat), group = stat(stat))) +
#'   stat_summary_wind(
#'     mapping = aes(wd = wd, ws = ws, z = NOx),
#'     geom = "polygon", position = "identity",
#'     fun = f,
#'     ws_cutfun = function(x) factor("ws"),
#'     fill = NA,
#'     size = 1
#'   ) +
#'   coord_radar(start = - 22.5 / 180 * pi) +
#'   scale_y_continuous(limits = c(0, NA), expand = c(0,0)) +
#'   scale_color_viridis_d(end = 0.8)
#'
#' # ggplot2 doesn't support faceting over a Variable computed in a stat.
#' # But we can summarise the data first and then create the plot.
#' # Less ggplot2 magic, but more transparent for the user
#' data_summarized <- summary_wind(data, ws, wd, NOx, fun = f,
#'   ws_cutfun = function(x) factor("ws")
#' )
#'
#' ggplot(data_summarized, aes(x = wd, y = NOx, color = stat, group = stat)) +
#'   geom_polygon(size = 1, fill = NA) +
#'   coord_radar(start = - 22.5 / 180 * pi ) +
#'   scale_color_viridis_d(end = 0.8) +
#'   scale_y_continuous(limits = c(0, NA), expand = c(0,0)) +
#'   facet_wrap(vars(stat))
#'
#' # like faceting the mapping mechanism makes it hard to impossible
#' # to use the grouping argument of summary wind. Do the summarise
#' # external.
#' # For example: how often comes which concentration from a sector
#' data_summarized <- summary_wind(data, ws, wd, NOx,
#'   groupings = groups(
#'     fNOx = ggplot2::cut_number(NO2, 5),
#'     year = lubridate::year(date)
#'   ),
#'   ws_cutfun = cut_number.fun(1)
#' )
#'
#' ggplot(data_summarized, aes(x = wd, y = freq, fill = forcats::fct_rev(fNOx))) +
#'   geom_bar(stat = "identity") +
#'   coord_polar2(start = - 22.5 / 180 * pi ) +
#'   scale_fill_viridis_d(direction = -1, name = "NOx")
geom_bar_wind <- function(mapping = NULL, data = NULL, stat = "summary_wind", position = "stack",
                          ...,
                          groupings = groups(),
                          fun = "mean",
                          fun.args = list(),
                          nmin = 3,
                          wd_cutfun = cut_wd.fun(binwidth = 45),
                          ws_cutfun = cut_ws.fun(binwidth = 1),
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = stat,,
    geom = GeomBarWind,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      groupings = groupings,
      fun = fun,
      fun.args = fun.args,
      nmin = nmin,
      wd_cutfun = wd_cutfun,
      ws_cutfun = ws_cutfun,
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname rOstluft-ggproto
#' @export
GeomBarWind <- ggproto("GeomBarWind", GeomRect,
  required_aes = c("x", "y"),

  # These aes columns are created by setup_data(). They need to be listed here so
  # that GeomRect$handle_na() properly removes any bars that fall outside the defined
  # limits, not just those for which x and y are outside the limits
  non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),

  setup_data = function(data, params) {
    data$width <- data$width %||%
      params$width %||% (ggplot2::resolution(data$x, FALSE) * 0.9)
    transform(data,
      ymin = pmin(y, 0), ymax = pmax(y, 0),
      xmin = as.numeric(x) - width / 2, xmax = as.numeric(x) + width / 2, width = NULL
    )
  },

  draw_panel = function(self, data, panel_params, coord, width = NULL) {
    # Hack to ensure that width is detected as a parameter
    ggproto_parent(GeomRect, self)$draw_panel(data, panel_params, coord)
  }
)
