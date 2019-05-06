stat_summary_wind <- function (mapping = NULL, data = NULL, geom = "raster", position = "identity",
                               bins = 100, fun = "mean", fun.args = list(), show.legend = NA, inherit.aes = TRUE, 
                               nmin = 1, ws_max = Inf, smooth = TRUE, k = 100, extrapolate = TRUE, dist = 0.1, ...) {
  
  layer(stat = StatWind, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(fun = fun, fun.args = fun.args, nmin = nmin, ws_max = ws_max, 
                      smooth = smooth, k = k, extrapolate = extrapolate, dist = dist, bins = bins, ...)
  )
}
