
#' Summarise y values over binned wind data.
#' 
#' Input data should be original unbinned data.
#' Depending on the groups argument, binning is either done 2-dimensional over cartesian u and v wind vectors 
#' (calculated from input data; then, stat_summary_wind() yields results similar to openair::polarplot()) 
#' or,
#' 1-dimensional over wind direction or wind velocity bins, respectively.
#' 
#' @param data a data.frame or tibble containing the data (wide format)
#' @param ws string giving the wind velocity parameter name (wind velocity preferably in m/s)
#' @param wd string giving the wind direction parameter name  in degrees
#' @param z string giving the parameter name to be summarised
#' @param groups can be NULL, c("u", "v"), "wd_class", "ws_class", ...
#' @param fun function or list of functions for summary.
#' @param fun.args A list of extra arguments to pass to fun.
#' @param nmin Minimum number of values for fun, if n < nmin: NA is returned
#' @param ws_max Maximum wind velocity for binning: above ws_max, z is set NA; can be NA
#' @param bins number of bins over the range of values if !groups %in% c("u", "v")
#' @param wd_binwidth width of bins (in degree) if groups == "wd_class"
#' @param wd_offset offset for wind_direction (in degree) if groups == "wd_class"; bins are then calculated over (wd + wd_offset) %% 360
#' @param ws_binwidth width of bins for wind velocity if groups == "ws_class"
#' @param smooth TRUE/FALSE, applies if groups = c("u", "v"); should smoothing of summary results should be performed
#' using fit_gam_surface()?
#' @param k numeric, applies if smooth = TRUE; degree of smoothing in fit_gam_surface()
#' @param extrapolate TRUE/FALSE, applies if smooth = TRUE; fit_gam_surface() returns extrapolated values for u, v coordinates that have NA for summarised z
#' if extrapolate = TRUE, those values are returned (to a certain degree depending on the value of dist)
#' @param dist numeric, fraction of 1, applies if smooth = TRUE and extrapolate = TRUE; maximum distance to coordinate-pair at which the result of 
#' gem_smooth(z) should be returned
#' 
#' @return a tibble with summarised data
#' 
#' Computed variables
#' 
#' * If groups = c("u", "v"): a tibble is returned, binned over u and v, with variables:
#' - wd: wind direction corresponding to midpoint value of u and v
#' - ws: wind velocity corresponding to midpoint value of u and v
#' - wd_class: new bins over wd considering wd_binwidth
#' - ws_class: new bins over ws considering ws_binwidth and ws_max
#' - u: bins over u (from input wd and ws)
#' - v: bins over v (from input wd and ws)
#' - z: result from fun(z, ...)
#' * If groups = NULL: groups = "wd". In this case, bins are calculated over wind direction; 
#' a tibble including wd_class and summarised z is returned
#' * groups can be strings for other varibables in data; then fun is applied over those; 
#' a tibble including groups and summarised z is returned
#' 
#' @export
stat_bin_wind <- function(data, ws, wd, z, groups = NULL, fun = "mean", fun.args = list(), nmin = 3, ws_max = Inf, 
                          wd_binwidth = 45, wd_offset = 0, ws_binwidth = 1) {
  
  if (is.null(groups)) groups <- "wd_class"
  ns <- function(x, ...) {sum(!is.na(x))}
  fun <- as.list(c(unlist(fun), "ns")) 
  data <-  
    data %>% 
    dplyr::mutate(
      wd_class = wd_classes(!!sym(wd), wd_binwidth),
      ws_class = cut(!!sym(ws), breaks = seq(0, max(pmin(!!sym(ws), ws_max, na.rm = TRUE), na.rm = TRUE), ws_binwidth))
    ) %>% 
    na.omit() %>% 
    dplyr::group_by_at(groups) %>%
    dplyr::summarise_at(
      .vars = z,
      .funs = fun,
      !!!fun.args
    ) %>%
    dplyr::ungroup() %>%
    tidyr::gather(stat, !!sym(z), -groups, -ns) %>% 
    dplyr::mutate(stat = factor(stat)) %>% 
    dplyr::filter(
      ns >= nmin
    )
  
  return(data)
}

