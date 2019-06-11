#' ggplot wrapper to create a wind-based polar plot
#'
#' @description Can mimic openair::polarplot(), i.e. wind direction / wind velocity heatmap,
#' including options for smoothing, flexible grid setting,
#' flexible stat functions, data extrapolation and plotting over a raster map.
#'
#' @return ggplot object
#
#' @examples
#' require(rOstluft)
#' require(rOstluft.data)
#' require(rOstluft.plot)
#' require(lubridate)
#' require(ggplot2)
#' require(dplyr)
#'
#' df <-
#'   rOstluft::read_airmo_csv(system.file("extdata", "Zch_Stampfenbachstrasse_2010-2014.csv", package = "rOstluft.data", mustWork = TRUE)) %>%
#'   rOstluft::rolf_to_openair() %>%
#'   dplyr::mutate(wday = lubridate::wday(date, label = TRUE, week_start = 1))
#'
#' # simple
#' ggpolarplot(df, aes(wd = wd, ws = ws, z = NOx))
#' # change smoothing degree
#' ggpolarplot(df, aes(wd = wd, ws = ws, z = NOx), k = 25)
#' # no data extrapolation
#' ggpolarplot(df, aes(wd = wd, ws = ws, z = NOx), k = 25, extrapolate = FALSE)
#' # no smoothing
#' ggpolarplot(df, aes(wd = wd, ws = ws, z = NOx), smooth = FALSE)
#' # cap maximum NOx concentration and wind velocity
#' ggpolarplot(df, aes(wd = wd, ws = ws, z = pmin(NOx, 40)), ws_max = 4, smooth = FALSE)
#' # change binning parameters
#' ggpolarplot(df, aes(wd = wd, ws = ws, z = NOx), smooth = FALSE, bins = 50, nmin = 10)
#' # facetting
#' ggpolarplot(df, aes(wd = wd, ws = ws, z = pmin(NOx, 50)), ws_max = 4, bins = 50, k = 25) +
#'   facet_wrap(wday~., scales = "fixed")
#' # different stat fun
#' ggpolarplot(df, aes(wd = wd, ws = ws, z = NOx), fun = "quantile", fun.args = list(probs = 0.95, na.rm = TRUE))
#' # facetting by stat
#' ggpolarplot(df, aes(wd = wd, ws = ws, z = NOx), fun = list("mean", "median"), ws_max = 4, bins = 50, k = 25) +
#'   facet_wrap(stat~.) # => hm, warum funzt das nicht??
#'
#' @export
ggpolarplot <- function(data,
                        mapping,
                        ...,
                        nmin = 3,
                        fun = "mean",
                        fun.args = list(na.rm = TRUE),
                        ws_max = NA,
                        smooth = TRUE,
                        k = 100,
                        extrapolate = TRUE,
                        dist = 0.1,
                        bins = 75,
                        fill_scale = scale_fill_gradientn(colours = matlab::jet.colors(100), na.value = NA),
                        ws_unit = "m/s",
                        breaks = waiver(),
                        bg = NULL
) {

  plot <-
    ggplot(data, mapping) +
    stat_summary_wind_2d(
      mapping = aes(x = stat(u), y = stat(v), fill = stat(z)),
      ...,
      fun = fun, fun.args = fun.args, nmin = nmin, ws_max = ws_max,
      smooth = smooth, k = k, extrapolate = extrapolate, dist = dist, bins = bins, groups = NULL
    ) +
    geom_point(aes(x = 0, y = 0), inherit.aes = FALSE, shape = 3, color = "gray80") +
    scale_y_continuous(breaks = breaks, labels = function(ws) paste0(abs(as.numeric(ws))," ",ws_unit)) +
    scale_x_continuous(breaks = breaks) +
    guides(fill = guide_colorbar(title = rlang::quo_text(mapping$z))) +
    fill_scale +
    coord_cartpolar(bg = bg) +
    ylab("wind speed") +
    theme_polarplot

  return(plot)
}







