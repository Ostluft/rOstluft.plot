#' ggplot2 wrapper to create a wind-based polar plot
#'
#' @description Inspired and derived from [openair::polarPlot()], i.e. applying a stat function on data to yield a wind direction +
#' wind velocity heatmap, (in cartesian u, v wind component space); includes options for smoothing, flexible grid
#' setting, flexible stat functions, data extrapolation and plotting over a raster map.
#'
#' @param data tibble containing wind speed, wind direction and air pollutant data
#' @param pixels number of bins at the 2-dimensional u, v wind component coordinate system
#' @param fill_scale ggplot2 continuous fill scale, e.g. [scale_fill_gradientn()]. If prefered the
#'   scale can added later to the returned ggplot object, but a message that the scale is replaced
#'   will be printed.
#' @param ylabels function to format ylabels. Default adds unit " m/s"
#' @param breaks waiver() or numeric vector, provides y-axis breaks
#' @param bg raster map, e.g. ggmap object as plot background
#' @param ... Other arguments passed on to [ggplot2::geom_raster()]. Used
#'   to set an aesthetic to a fixed value
#'
#' @inheritParams summary_wind_2d
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
#' df <- rOstluft::read_airmo_csv(fn) %>%
#'   rOstluft::rolf_to_openair() %>%
#'   dplyr::mutate(wday = lubridate::wday(date, label = TRUE, week_start = 1))
#'
#' # simple
#' ggpolarplot(df, wd = wd, ws = ws, z = NOx)
#'
#' # change smoothing degree, add custom breaks and change grid style, different fill scale
#' fs <- scale_fill_viridis_c(na.value = NA)
#' ggpolarplot(df, wd = wd, ws = ws, z = NOx, k = 25, fill_scale = fs, breaks = seq(0,8,2))  +
#'   theme(panel.grid.major = element_line(linetype = 2, size = 0.25, color = "gray80"))
#'
#'
#' # no data extrapolation
#' ggpolarplot(df, wd = wd, ws = ws, z = NOx, extrapolate = FALSE, breaks = seq(0,8,2))
#'
#' # no smoothing
#' ggpolarplot(df, wd = wd, ws = ws, z = NOx, smooth = FALSE, breaks = seq(0,8,2))
#'
#' # squish maximum NOx concentration and cut off wind velocity at 4 m/s
#' fs = scale_fill_viridis_squished(limits = c(0,50), breaks = seq(0,50,10), na.value = NA)
#' ggpolarplot(df, wd = wd, ws = ws, z = NOx, ws_max = 4,  fill_scale = fs,
#'             smooth = FALSE, breaks = c(0, 2, 4))
#'
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
#' # facetting variable must be included in grp
#' ggpolarplot(df, wd = wd, ws = ws, z = NOx, groupings = grp(wday),
#'             ws_max = 4, pixels = 50^2, k = 25, breaks = c(0, 2, 4)) +
#'   facet_wrap(vars(wday))
#'
#' # different stat fun
#' ggpolarplot(df, wd = wd, ws = ws, z = NOx, fun = "quantile",
#'             fun.args = list(probs = 0.95, na.rm = TRUE),
#'             breaks = seq(0,10,2))
#'
#' # facetting by stat
#' ggpolarplot(df, wd = wd, ws = ws, z = NOx, fun = list("mean", "median"), ws_max = 4,
#'              pixels = 50^2, k = 25, breaks = c(0, 2, 4)) +
#'   facet_wrap(vars(stat))
#'
#' # background map, just remember the location on the map has no correlation with
#' # the overlaying data. Only the direction.
#' bb <- bbox_lv95(2683141, 1249040, 500)
#' bg <- get_stamen_map(bb)
#' fs <- scale_fill_gradientn_squished(
#'   limits = c(0,50), breaks = seq(0,50,10),
#'   na.value = NA, colors = matlab::jet.colors(20)
#' )
#'
#' ggpolarplot(df, wd = wd, ws = ws, z = NOx, ws_max = 4,
#'             bg = bg, alpha = 0.6,
#'             fill_scale = fs, smooth = TRUE, breaks = c(0,2,4)) +
#'   theme(
#'     panel.grid.major = element_line(linetype = 2, color = "black", size = 0.5)
#'   )
#'
ggpolarplot <- function(data, ws, wd, z,
                        groupings = grp(),
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
                        bg = NULL,
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
    ggplot(data_summarized, aes(x = .data$u, y = .data$v, fill = !!z)) +
    geom_raster(...) +
    scale_y_continuous(breaks = breaks, labels = ylabels, expand = ggplot2::expand_scale(add = 0.5)) +
    coord_cartpolar(limit = ws_max, bg = bg, grid = "foreground") +
    guides(fill = guide_colorbar(title = rlang::quo_text(z))) +
    fill_scale +
    theme_rop_polarplot()

  return(plot)
}







