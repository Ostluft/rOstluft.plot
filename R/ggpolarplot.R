#' ggplot2 wrapper to create a wind-based polar plot
#'
#' @description Inspired and derived from [openair::polarPlot()], i.e. applying a stat function on data to yield a wind direction +
#' wind velocity heatmap, (in cartesian u, v wind component space); includes options for smoothing, flexible grid
#' setting, flexible stat functions, data extrapolation and plotting over a raster map.
#'
#' @param data tibble containing wind speed, wind direction and air pollutant data
#' @param pixels number of bins at the cartesian 2-dimensional u, v wind component coordinate system
#' @param fill_scale ggplot2 continuous fill scale, e.g. [scale_fill_gradientn()]
#' @param ylabels function to format ylabels. Default adds unit " m/s"
#' @param breaks waiver() or numeric vector, provides y-axis breaks
#' @param bg raster map, e.g. ggmap object as plot background
#' @inheritParams summary_wind_2d
#'
#' @return [ggplot2::ggplot()] object
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_2010-2014.csv")
#'
#' df <- rOstluft::read_airmo_csv(fn) %>%
#'   rOstluft::rolf_to_openair() %>%
#'   dplyr::mutate(wday = lubridate::wday(date, label = TRUE, week_start = 1))
#'
#' # simple
#' ggpolarplot(df, wd = wd, ws = ws, z = NOx)
#'
#' # change smoothing degree, add custom breaks and change grid style, different fill scale
#' ggpolarplot(df, wd = wd, ws = ws, z = NOx, k = 25, breaks = seq(0,10,2))  +
#'   theme(panel.grid.major = element_line(linetype = 2, size = 0.25, color = "gray80")) +
#'   scale_fill_viridis_c(na.value = NA)
#'
#' # no data extrapolation
#' ggpolarplot(df, wd = wd, ws = ws, z = NOx, extrapolate = FALSE, breaks = seq(0,8,2))
#'
#' # no smoothing
#' ggpolarplot(df, wd = wd, ws = ws, z = NOx, smooth = FALSE, breaks = seq(0,8,2))
#'
#' # cap maximum NOx concentration and wind velocity
#' ggpolarplot(df, wd = wd, ws = ws, z = NOx, ws_max = 4, smooth = FALSE, breaks = 0:4) +
#'   scale_fill_viridis_squished(limits = c(0,50), breaks = seq(0,50,10), na.value = NA)
#'
#' # like jet colors better, want it smoothed?
#' ggpolarplot(df, wd = wd, ws = ws, z = NOx, ws_max = 4, smooth = TRUE, breaks = 0:4) +
#'   scale_fill_gradientn_squished(limits = c(0,50), breaks = seq(0,50,10),
#'                                 na.value = NA, colors = matlab::jet.colors(100))
#'
#' # change binning parameters
#' ggpolarplot(df, wd = wd, ws = ws, z = NOx, smooth = FALSE, pixels = 50^2,
#'             nmin = 10, breaks = seq(0,6,2))
#'
#' # facetting variable must be included in groups
#' ggpolarplot(df, wd = wd, ws = ws, z = NOx, groupings = groups(wday),
#'             ws_max = 4, pixels = 50^2, k = 25, breaks = 0:4) +
#'   facet_wrap(vars(wday), scales = "fixed")
#'
#' # different stat fun
#' ggpolarplot(df, wd = wd, ws = ws, z = NOx, fun = "quantile",
#'             fun.args = list(probs = 0.95, na.rm = TRUE),
#'             breaks = seq(0,10,2))
#'
#' # facetting by stat
#' ggpolarplot(df, wd = wd, ws = ws, z = NOx, fun = list("mean", "median"), ws_max = 4,
#'              pixels = 50^2, k = 25, breaks = seq(0,10,2)) +
#'   facet_wrap(vars(stat))
ggpolarplot <- function(data, ws, wd, z,
                        groupings = groups(),
                        fun = "mean",
                        fun.args = list(na.rm = TRUE),
                        nmin = 3,
                        ws_max = NA,
                        smooth = TRUE,
                        k = 200,
                        extrapolate = TRUE,
                        dist = 0.1,
                        pixels = 80^2,
                        fill_scale = scale_fill_gradientn(colours = matlab::jet.colors(20), na.value = NA),
                        ylabels = scales::unit_format(unit = "m/s"),
                        breaks = waiver(),
                        bg = NA,
                        ...
) {

  ws <- rlang::ensym(ws)
  wd <- rlang::ensym(wd)
  z <- rlang::ensym(z)

  data_summarized <- summary_wind_2d(data, !!ws, !!wd, !!z, groupings = groupings,
                                     fun = fun, fun.args = fun.args, nmin = nmin,
                                     ws_max = ws_max, bins = pixels, smooth = smooth, k = k,
                                     extrapolate = extrapolate, dist = dist)

  plot <-
    ggplot(data_summarized, aes(x = u, y = v, fill = !!z)) +
    annotation_raster(bg, -Inf, Inf, -Inf, Inf) +
    geom_raster() +
    scale_y_continuous(breaks = breaks, labels = ylabels) +
    scale_x_continuous(breaks = breaks) +
    coord_cartpolar(expand = FALSE) +
    guides(fill = guide_colorbar(title = rlang::quo_text(z))) +
    fill_scale +
    theme_polarplot

  return(plot)
}







