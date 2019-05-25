
#' Summarise y values over binned wind data, split into u and v components.
#' 
#' Input data should be original unbinned data including wind direction and wind velocity;
#' binning is done 2-dimensional over cartesian u and v wind vectors 
#' 
#' @param data a data.frame or tibble containing the data (wide format)
#' @param ws string giving the wind velocity parameter name (wind velocity preferably in m/s)
#' @param wd string giving the wind direction parameter name  in degrees
#' @param z string giving the parameter name to be summarised
#' @param groups can be NULL, c("u", "v"), ...
#' @param fun function or list of functions for summary.
#' @param fun.args A list of extra arguments to pass to fun.
#' @param nmin Minimum number of values for fun, if n < nmin: NA is returned
#' @param ws_max Maximum wind velocity for binning: above ws_max, z is set NA; can be NA
#' @param bins number of bins over the range of values in c(u, v)
#' @param smooth TRUE/FALSE, should smoothing of summary results should be performed
#' using fit_gam_surface()?
#' @param k numeric, applies if smooth = TRUE; degree of smoothing in fit_gam_surface()
#' @param extrapolate TRUE/FALSE, applies if smooth = TRUE; fit_gam_surface() returns extrapolated values for u, v coordinates that have NA for summarised z
#' if extrapolate = TRUE, those values are returned (to a certain degree depending on the value of dist)
#' @param dist numeric, fraction of 1, applies if smooth = TRUE and extrapolate = TRUE; maximum distance to coordinate-pair at which the result of 
#' z should be returned
#' 
#' @return a tibble with summarised data along u and v wind vectors
#' 
#' Computed variables
#' 
#' * a tibble is returned, binned over u and v, with variables:
#' - wd: wind direction corresponding to midpoint value of u and v
#' - ws: wind velocity corresponding to midpoint value of u and v
#' - u: bins over u (from input wd and ws)
#' - v: bins over v (from input wd and ws)
#' - z: result from fun(z, ...)
#' 
#' @export
stat_bin_wind_2d <- function(data, ws, wd, z, groups = NULL, fun = "mean", fun.args = list(), nmin = 3, ws_max = NA, bins = 100, 
                             smooth = TRUE, k = 100, extrapolate = TRUE, dist = 0.1) {
  
  if (is.null(groups)) groups <- c("u", "v")
  ns <- function(x, ...) {sum(!is.na(x))}
  fun <- c(unlist(fun), "ns")
  fun <- rlang::set_names(as.list(fun), fun)
  data <- 
    data %>%
    dplyr::mutate(
      u = !!rlang::sym(ws) * sin(pi * !!rlang::sym(wd) / 180),
      v = !!rlang::sym(ws) * cos(pi * !!rlang::sym(wd) / 180)
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
    tidyr::gather(stat, !!z, -!!groups, -ns) %>% 
    dplyr::mutate(
      u = midpoints(u),
      v = midpoints(v),
      stat = factor(stat)
      ) %>% 
    dplyr::filter(
      ns >= nmin
    ) %>% 
    dplyr::rename(
      n = ns
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
      dplyr::do({
        fit_gam_surface(., x = "u", y = "v", z = z, weights = pmin(3, .$n) / 3,
                        k = k, extrapolate = extrapolate, dist = dist)
      }) %>% 
      dplyr::ungroup()
    data <- 
      dplyr::bind_cols(
        data_smooth,
        dplyr::select(data, -u, -v, -!!rlang::sym(z), -stat, -n)
      )
  }
  data <- 
    data %>% 
    dplyr::mutate(
      !!wd := uv2wd(u, v),
      !!ws := sqrt(u^2 + v^2),
      !!z := ifelse(!!rlang::sym(ws) > ws_max, NA, !!rlang::sym(z))
    )
  
  return(data)
}

