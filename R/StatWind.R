#' ggproto for stat_bin_wind()
#' 
#' @export
StatWind <- ggproto("StatWind", Stat,
                    
                    compute_group = function(data, scales, fun = "mean", fun.args = list(), nmin = 3, ws_max = Inf,
                                             smooth = TRUE, k = 100, extrapolate = TRUE, dist = 0.1, bins = 100, 
                                             wd_binwidth = 45, wd_offset = 0, ws_binwidth = 1, groups = NULL, ...) {
                      stat_bin_wind(wd = data$wd, ws = data$ws, y = data$y, fun = fun, fun.args = fun.args, nmin = nmin, 
                                    ws_max = ws_max, smooth = smooth, k = k, extrapolate = extrapolate, 
                                    dist = dist, bins = bins, wd_binwidth = wd_binwidth, wd_offset = wd_offset, ws_binwidth = ws_binwidth, groups = groups, ...)
                    },
                    
                    required_aes = c("wd", "ws", "y")
)
