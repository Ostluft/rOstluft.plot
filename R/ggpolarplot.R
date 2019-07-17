#' ggplot wrapper to create a wind-based polar plot
#'
#' @description Can mimic openair::polarplot(), i.e. wind direction - wind velocity heatmap,
#' including options for smoothing, flexible grid setting,
#' flexible stat functions, data extrapolation and plotting over a raster map.
#'
#' @param data tibble containing wind speed, wind direction and air pollutant concentration data
#' @param mapping ggplot2 mapping, e.g. aes(wd = wd, ws = ws, z = NOx); requires wd, ws, z
#' @param nmin numeric, minimum number of data points to be averaged in one wind speed / wind direction bin
#' @param fun character string, stat function to be applied at wind speed / wind direction bins
#' @param fun.args list, arguments to fun
#' @param ws_max maximum wind speed considered for plotting
#' @param smooth TRUE/FALSE, should the result of stat_summary_wind_2d() be smoothed using fit_gam_surface() (with mgcv::bam())?
#' @param k numeric, smoothing parameter for formular smoothing term s(..., k = k) in mgcv::bam()
#' @param extrapolate TRUE/FALSE, should smoothed surfaced be extended further than bins with actual data? (if smooth == TRUE)
#' @param dist mnumeric < 1, maximum distance from bin at which extrapolation is done (if extrapolate == TRUE). dist is relative, i.e. 0.1 = 10% of total range
#' @param pixels number of bins at the 2-dimensional u, v wind component coordinate system
#' @param fill_scale ggplot2 fill scale, e.g. scale_fill_gradientn(...)
#' @param ws_unit character, y-axis labels are parsed with ws_unit; can be NULL
#' @param breaks waiver() or numeric vector, provides y-axis breaks
#' @param bg raster map, e.g. ggmap object as plot background
#'
#' @return ggplot object
#'
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
#' ggpolarplot(df, aes(wd = wd, ws = ws, z = NOx), extrapolate = FALSE)
#' # no smoothing
#' ggpolarplot(df, aes(wd = wd, ws = ws, z = NOx), smooth = FALSE)
#' # cap maximum NOx concentration and wind velocity
#' ggpolarplot(df, aes(wd = wd, ws = ws, z = pmin(NOx, 40)), ws_max = 4, smooth = FALSE)
#' # change binning parameters
#' ggpolarplot(df, aes(wd = wd, ws = ws, z = NOx), smooth = FALSE, pixels = 50^2, nmin = 10)
#' # facetting
#' ggpolarplot(df, aes(wd = wd, ws = ws, z = pmin(NOx, 50)), ws_max = 4, pixels = 50^2, k = 25) +
#'   facet_wrap(wday~., scales = "fixed")
#' # different stat fun
#' ggpolarplot(df, aes(wd = wd, ws = ws, z = NOx), fun = "quantile", fun.args = list(probs = 0.95, na.rm = TRUE))
#' # facetting by stat
#' ggpolarplot(df, aes(wd = wd, ws = ws, z = NOx), fun = list("mean", "median"), ws_max = 4, pixels = 50^2, k = 25) +
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
                        k = 200,
                        extrapolate = TRUE,
                        dist = 0.1,
                        pixels = 80^2,
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
      smooth = smooth, k = k, extrapolate = extrapolate, dist = dist, bins = pixels, groups = NULL
    ) +
    geom_point(aes(x = 0, y = 0), inherit.aes = FALSE, shape = 3, color = "gray80") +
    scale_y_continuous(breaks = breaks, labels = scales::unit_format(unit = ws_unit)) +
    scale_x_continuous(breaks = breaks) +
    guides(fill = guide_colorbar(title = rlang::quo_text(mapping$z))) +
    fill_scale +
    coord_cartpolar(bg = bg) +
    ylab("wind speed") +
    theme_polarplot

  return(plot)
}







