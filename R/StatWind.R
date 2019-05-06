StatWind <- ggproto("StatWind", Stat,
                    
                    compute_group = function(data, scales, fun = "mean", fun.args = list(), nmin = 3, ws_max = Inf,
                                             smooth = TRUE, k = 100, extrapolate = TRUE, dist = 0.1, bins = 100, ...) {
                      stat_bin_wind(wd = data$wd, ws = data$ws, z = data$z, fun = fun, fun.args = fun.args, nmin = nmin, 
                                    ws_max = ws_max, smooth = smooth, k = k, extrapolate = extrapolate, 
                                    dist = dist, bins = bins, ...)
                    },
                    
                    required_aes = c("wd", "ws", "z")
)
