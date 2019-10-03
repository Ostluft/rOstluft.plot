#' Summarise z values over binned wind data.
#'
#' @description Binning is done by StatWind, so input data to[stat_summary_wind()] should be original unbinned data.
#' Depending on the groups argument, binning is either done 2-dimensional over cartesian u and v wind vectors
#' (calculated from input data; then, [stat_summary_wind()] yields results similar to [openair::polarPlot()])
#' or 1-dimensional over wind direction or wind velocity bins, respectively.
#'
#' @param mapping  ggplot2 mapping, e.g. aes(wd = wd, ws = ws, z = NOx); requires wd, ws, z
#' @param data The data to be displayed in this layer. Requires input data including at least three
#'   columns carrying information regarding:
#'     * wind direction (in °)
#'     * wind velocity
#'     * z-values (e.g. air pollutant concentration)
#' @param fun function or list of functions for summary.
#' @param ... other arguments passed on to [ggplot2::layer()] as `params = list(...)`.
#' @param fun.args a list of extra arguments to pass to fun.
#' @param nmin numeric, minimum number of values for fun, if n < nmin: NA is returned
#' @param ws_max numeric or NA, maximum wind velocity for binning: above ws_max, z is set NA
#' @param bins numeric, number of bins over the range of values if `!groups %in% c("u", "v")`
#' @param smooth TRUE/FALSE, applies if `groups = c("u", "v")`; should smoothing of summary results should be performed
#'   using [fit_gam_surface()]?
#' @param k numeric, applies if smooth = TRUE; degree of smoothing in smooth term in fit_gam_surface()
#' @param extrapolate TRUE/FALSE, applies if smooth = TRUE; fit_gam_surface() returns extrapolated (predicted)
#'   values for u, v coordinates that otherwise would have have NA for summarised z if extrapolate = TRUE,
#'   those values are returned (to a certain degree depending on the value of dist)
#' @param dist numeric, fraction of 1, applies if smooth = TRUE and extrapolate = TRUE; maximum distance to next
#'   coordinate-pair at which the result of fit_gam_surface(z) should be returned
#' @param geom The geometric object to use display the data (in this case: raster).
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @inheritParams ggplot2::layer
#'
#' @return ggplot2 layer
#'
#' @section Aesthetics:
#'
#' * wd: wind direction in degrees
#' * ws: wind velocity
#' * z: z values to be summarised
#'
#' @section Computed variables:
#'
#' * If groups = c("u", "v"): a tibble is returned, binned over u and v, with variables:
#' - wd: wind direction corresponding to midpoint value of u and v
#' - ws: wind velocity corresponding to midpoint value of u and v
#' - wd_class: new bins over wd considering binwidth
#' - ws_class: new bins over ws considering binwidth and ws_max
#' - u: bins over u (from input wd and ws)
#' - v: bins over v (from input wd and ws)
#' - z: result from fun(z, ...)
#' * If groups = NULL: groups = "wd". In this case, bins are calculated over wind direction;
#'   a tibble including wd_class and summarised z is returned
#' * groups can be strings for other varibables in data; then fun is applied over those;
#'   a tibble including groups and summarised z is returned
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_2010-2014.csv")
#'
#' df <- rOstluft::read_airmo_csv(fn) %>%
#'   rOstluft::rolf_to_openair()
#'
#' ggplot(df, aes(x = stat(u), y = stat(v), fill = stat(z))) +
#'   stat_summary_wind_2d(mapping = aes(wd = wd, ws = ws, z = NO2), bins = 50^2) +
#'   coord_cartpolar() +
#'   scale_fill_viridis_c(na.value = NA)
stat_summary_wind_2d <- function (data = NULL, mapping = NULL, geom = "raster", position = "identity",
                                  ...,
                                  fun = "mean",
                                  fun.args = list(),
                                  nmin = 1,
                                  ws_max = NA,
                                  bins = 10^2,
                                  smooth = TRUE,
                                  k = 100,
                                  extrapolate = TRUE,
                                  dist = 0.1,
                                  na.rm = FALSE,
                                  show.legend = NA,
                                  inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = StatSummaryWind2d,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      fun.args = fun.args,
      nmin = nmin,
      ws_max = ws_max,
      bins = bins,
      smooth = smooth,
      k = k,
      extrapolate = extrapolate,
      dist = dist,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname rOstluft-ggproto
#' @export
StatSummaryWind2d <- ggproto("StatSummaryWind2d", Stat,
  compute_group = function(data, scales,
                           fun = "mean",
                           fun.args = list(),
                           nmin = 1,
                           ws_max = NA,
                           bins = 10^2,
                           smooth = TRUE,
                           k = 100,
                           extrapolate = TRUE,
                           dist = 0.1) {

    summary_wind_2d(data = data, wd = "wd", ws = "ws", z = "z", fun = fun, fun.args = fun.args, nmin = nmin,
                    ws_max = ws_max, smooth = smooth, k = k, extrapolate = extrapolate,
                    dist = dist, bins = bins)

  },
  required_aes = c("wd", "ws", "z")
)










