
#' Summarise y values over binned wind data.
#' 
#' Input data should be original unbinned data.
#' 1-dimensional binning and calculating symmary statistics over wind direction and/or wind velocity bins, respectively.
#' 
#' @param data a data.frame or tibble containing the data (wide format)
#' @param ws string giving the wind velocity parameter name (wind velocity preferably in m/s)
#' @param wd string giving the wind direction parameter name  in degrees
#' @param z string giving the parameter name to be summarised
#' @param groups can be NULL, wd, ws, ...
#' @param fun function or list of functions for summary.
#' @param fun.args A list of extra arguments to pass to fun.
#' @param nmin Minimum number of values for fun, if n < nmin: NA is returned
#' @param ws_max Maximum wind velocity for binning: above ws_max, z is set NA; can be NA
#' @param wd_binwidth width of bins (in degree) if groups = wd
#' @param wd_offset offset for wind_direction (in degree) if groups = wd; bins are then calculated over (wd + wd_offset) %% 360
#' @param ws_binwidth width of bins for wind velocity if groups = ws
#' 
#' @return a tibble with summarised data
#' 
#' Computed variables
#' 
#' * If groups = NULL: groups = "wd". In this case, bins are calculated over wind direction; 
#' a tibble including wd and summarised z is returned
#' * groups can be strings for other variables in data; then fun is applied over those; 
#' a tibble including groups and summarised z is returned
#' 
#' @export
stat_bin_wind <- function(data, ws, wd, z, groups = NULL, fun = "mean", fun.args = list(), nmin = 3, ws_max = NA, 
                          wd_cutfun = function(wd) wd_classes(wd, wd_binwidth = 45), wd_offset = 0, 
                          ws_cutfun = function(ws) ws_classes(ws, ws_binwidth = 1)) {
  
  
  
  if (is.null(groups)) groups <- wd
  ns <- function(x, ...) {sum(!is.na(x))}
  fun <- c(unlist(fun), "ns")
  fun <- rlang::set_names(as.list(fun), fun)
  data <-  
    data %>% 
    dplyr::mutate(
      !!wd := wd_cutfun(!!rlang::sym(wd)),
      !!ws := wd_cutfun(!!rlang::sym(ws))
    ) %>% 
    na.omit() %>% 
    dplyr::group_by_at(groups) %>%
    dplyr::summarise_at(
      .vars = z,
      .funs = fun,
      !!!fun.args
    ) %>%
    dplyr::ungroup() %>%
    tidyr::gather(stat, !!z, -!!groups, -ns) %>% 
    dplyr::mutate(stat = factor(stat)) %>% 
    dplyr::filter(
      ns >= nmin
    ) %>% 
    dplyr::rename(
      n = ns
    )
  
  return(data)
}

