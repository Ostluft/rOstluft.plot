
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
stat_bin_wind_2d <- function(data, ws, wd, z, groups = NULL, fun = "mean", fun.args = list(), nmin = 3, ws_max = Inf, bins = 100, 
                          wd_binwidth = 45, wd_offset = 0, ws_binwidth = 1, smooth = TRUE, k = 100, extrapolate = TRUE, dist = 0.1) {
  
  if (is.null(groups)) groups <- c("u", "v")
  ns <- function(x, ...) {sum(!is.na(x))}
  fun <- as.list(c(unlist(fun), "ns")) 
  data <- 
    data %>%
    dplyr::mutate(
      u = !!sym(ws) * sin(pi * !!sym(wd) / 180),
      v = !!sym(ws) * cos(pi * !!sym(wd) / 180)
    ) 
  uv_max <- pmin(max(abs(c(data$u, data$v)), na.rm = TRUE), ws_max, na.rm = TRUE)
  uv_cuts <- seq(-uv_max, uv_max, length.out = bins)
  data <-  
    data %>% 
    dplyr::mutate(
      u = cut(u, breaks = uv_cuts),
      v = cut(v, breaks = uv_cuts)
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
  data <- 
    data %>% 
    dplyr::mutate(
      u = midpoints(u),
      v = midpoints(v)
    )
  data <-
    expand.grid(
      list(
        u = midpoints(cut(uv_cuts, breaks = uv_cuts)),
        v = midpoints(cut(uv_cuts, breaks = uv_cuts)),
        stat = unique((data$stat))
      )) %>% 
    na.omit() %>% 
    dplyr::tbl_df() %>% 
    dplyr::left_join(data, by = c("u", "v", "stat"))
  if (smooth) {
    data_smooth <- 
      data %>% 
      dplyr::group_by(stat) %>%
      do({
        # ?
        fit_gam_surface(u = .$u, v = .$v, z = .$!!z, weights = pmin(3, .$ns) / 3,
                        k = k, extrapolate = extrapolate, dist = dist)
      }) %>% 
      dplyr::ungroup()
    data <- 
      dplyr::bind_cols(
        data_smooth,
        dplyr::select(data, -u, -v, -!!sym(z), -stat)
      )
  }
  data <- 
    data %>% 
    dplyr::mutate(
      # !!wd := uv2wd(u, v),
      # !!ws := sqrt(u^2 + v^2),
      u = cut(!!sym(ws) * sin(pi * !!sym(wd) / 180), breaks = uv_cuts),
      v = cut(!!sym(ws) * cos(pi * !!sym(wd) / 180), breaks = uv_cuts),
      !!z := ifelse(!!sym(ws) > ws_max, NA, !!sym(z))
    )
  
  return(data)
}

