#' Summarise z values over binned wind data.
#'
#' @description Binning is done by StatWind, so input data to [stat_summary_wind()] should be original unbinned data.
#' Depending on the groups argument, binning is either done 2-dimensional over cartesian u and v wind vectors
#' (calculated from input data; then, [stat_summary_wind()] yields results similar to [openair::polarPlot()]
#' or, 1-dimensional over wind direction or wind velocity bins, respectively.
#'
#' @param mapping  ggplot2 mapping, e.g. aes(wd = wd, ws = ws, z = NOx); requires wd, ws, z
#' @param data The data to be displayed in this layer.
#'   requires input data including at least three columns carrying information regarding:
#'     * wind direction (in °)
#'     * wind velocity
#'     * z-values (e.g. air pollutant concentration)
#' @param fun function or list of functions for summary.
#' @param ... other arguments passed on to [ggplot2::layer()] as `params = list(...)`.
#' @param fun.args a list of extra arguments to pass to fun.
#' @param nmin numeric, minimum number of values for fun, if n < nmin: NA is returned
#' @param wd_cutfun function, cut function for wind direction (to create bins)
#' @param wd_offset numeric, offset for wind_direction (in °) if groups == "wd"; bins are then calculated
#'   over `(wd + wd_offset) %% 360`
#' @param ws_cutfun function, cut function for wind speed
#' @param groups character string, can be NULL, "wd", ...
#' @param geom The geometric object to use display the data.
#' @param layer_args named list, passed on to [ggplot2::layer()]
#' @param param_args named list, passed on to [ggplot2::layer()] as argument params after combining with the other
#'   arguments
#'
#' @return [ggplot2::layer()]
#'
#' @export
stat_summary_wind <- function (data = NULL, mapping = NULL, fun = "mean", fun.args = list(),
                               nmin = 1, wd_cutfun = cut_wd.fun(binwidth = 45), wd_offset = 0,
                               ws_cutfun = cut_ws.fun(binwidth = 1, ws_max = NA), groups = NULL,
                               layer_args = list(geom = "polygon"), param_args = list()) {

  params = c(list(fun = fun, fun.args = fun.args, nmin = nmin,
                wd_cutfun = wd_cutfun, wd_offset = wd_offset,
                ws_cutfun = ws_cutfun, groups = groups), param_args)

  rlang::exec(layer, stat = StatWind, data = data, mapping = mapping, position = "identity",
              params = params, !!!layer_args)
}




#' @rdname rOstluft-ggproto
#' @export
StatWind <- ggproto("StatWind", Stat,
  compute_group = function(data, scales, fun = "mean", fun.args = list(), nmin = 3,
                           wd_cutfun = cut_wd.fun(binwidth = 45), wd_offset = 0,
                           ws_cutfun = cut_ws.fun(binwidth = 1, ws_max = NA), groups = NULL) {

    summary_wind(data = data, wd = "wd", ws = "ws", z = "z", fun = fun, fun.args = fun.args, nmin = nmin,
                  wd_cutfun = wd_cutfun, wd_offset = wd_offset, ws_cutfun = ws_cutfun, groups = groups)
  },
  required_aes = c("wd", "ws", "z")
)










