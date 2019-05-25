#' Summarise z values over binned wind data.
#' 
#' @description Binning is done by StatWind, so input data to stat_summary_wind() should be original unbinned data.
#' Depending on the groups argument, binning is either done 2-dimensional over cartesian u and v wind vectors 
#' (calculated from input data; then, stat_summary_wind() yields results similar to openair::polarplot()) 
#' or,
#' 1-dimensional over wind direction or wind velocity bins, respectively.
#' 
#' @param mapping Set of aesthetic mappings created by aes() or aes_(). 
#' If specified and inherit.aes = TRUE (the default), it is combined with 
#' the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping..
#' @param data The data to be displayed in this layer. 
#' #' requires input data including at least three columns carrying information regarding: 
#' * wind direction 
#' * wind velocity
#' * z
#' @param geom The geometric object to use display the data.
#' @param fun function or list of functions for summary.
#' @param ... Other arguments passed on to layer(params = list(...)).
#' @param fun.args A list of extra arguments to pass to fun.
#' @param nmin Minimum number of values for fun, if n < nmin: NA is returned
#' @param ws_max Maximum wind velocity for binning: above ws_max, z is set NA
#' @param bins number of bins over the range of values if !groups %in% c("u", "v")
#' @param smooth TRUE/FALSE, applies if groups = c("u", "v"); should smoothing of summary results should be performed
#' using gam_surface()?
#' @param k numeric, applies if smooth = TRUE; degree of smoothing in gam_surface()
#' @param extrapolate TRUE/FALSE, applies if smooth = TRUE; gem_smooth() returns extrapolated values for u, v coordinates that have NA for summarised z
#' if extrapolate = TRUE, those values are returned (to a certain degree depending on the value of dist)
#' @param dist numeric, fraction of 1, applies if smooth = TRUE and extrapolate = TRUE; maximum distance to coordinate-pair at which the result of 
#' gem_smooth(z) should be returned
#' @param groups can be NULL, c("u", "v"), ...
#' 
#' @return ggplot2 layer
#' 
#' Aesthetics
#' 
#' * wd: wind direction in degrees
#' * ws: wind velocity
#' * z: z values to be summarised
#' 
#' Computed variables
#' 
#' * If groups = c("u", "v"): a tibble is returned, binned over u and v, with variables:
#' - wd: wind direction corresponding to midpoint value of u and v
#' - ws: wind velocity corresponding to midpoint value of u and v
#' - u: bins over u (from input wd and ws)
#' - v: bins over v (from input wd and ws)
#' - z: result from fun(z, ...)
#' * groups can be strings for other variables in data; then fun is applied over those; 
#' a tibble including groups and summarised z is returned
#' 
#' @export
stat_summary_wind <- function (data = NULL, mapping = NULL, geom = "polygon", position = "identity",
                               fun = "mean", fun.args = list(), show.legend = NA, inherit.aes = TRUE, 
                               nmin = 1, wd_cutfun = function(wd) wd_classes(wd, wd_binwidth = 45), wd_offset = 0, 
                               ws_cutfun = function(ws) ws_classes(ws, ws_binwidth = 1, ws_max = NA), groups = NULL, ...) {
  
  layer(stat = StatWind, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(fun = fun, fun.args = fun.args, nmin = nmin, 
                      wd_cutfun = wd_cutfun, wd_offset = wd_offset, 
                      ws_cutfun = ws_cutfun, groups = groups, ...)
  )
}




#' ggproto for stat_bin_wind()
#' 
#' @export
StatWind <- ggproto("StatWind", Stat,
                    
                    compute_group = function(data, scales, fun = "mean", fun.args = list(), nmin = 3, ws_max = NA,
                                             wd_cutfun = function(wd) wd_classes(wd, wd_binwidth = 45), wd_offset = 0, 
                                             ws_cutfun = function(ws) ws_classes(ws, ws_binwidth = 1, ws_max = NA), groups = NULL, ...) {
                      
                      # wd <- rlang::quo_text(mapping$wd)
                      # ws <- rlang::quo_text(mapping$ws)
                      # z <- rlang::quo_text(mapping$z)
                      
                      stat_bin_wind(data = data, wd = "wd", ws = "ws", z = "z", fun = fun, fun.args = fun.args, nmin = nmin, 
                                    ws_max = ws_max, wd_cutfun = wd_cutfun, wd_offset = wd_offset, 
                                    ws_cutfun = ws_cutfun, groups = groups)
                    },
                    
                    required_aes = c("wd", "ws", "z")
)










