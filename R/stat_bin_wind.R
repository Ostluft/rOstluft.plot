#' Summarise y values over binned wind data.
#' 
#' Input data should be original unbinned data.
#' Depending on the groups argument, binning is either done 2-dimensional over cartesian u and v wind vectors 
#' (calculated from input data; then, stat_summary_wind() yields results similar to openair::polarplot()) 
#' or,
#' 1-dimensional over wind direction or wind velocity bins, respectively.
#' 
#' @param ws wind velocity (preferably in m/s)
#' @param wd wind direction in degrees
#' @param y y values to be summarised
#' @param groups can be NULL, c("u_class", "v_class"), "wd_class", "ws_class", ...
#' @param fun function or list of functions for summary.
#' @param fun.args A list of extra arguments to pass to fun.
#' @param nmin Minimum number of values for fun, if n < nmin: NA is returned
#' @param ws_max Maximum wind velocity for binning: above ws_max, y is set NA
#' @param bins number of bins over the range of values if !groups %in% c("u_class", "v_class")
#' @param wd_binwidth width of bins (in degree) if groups == "wd_class"
#' @param wd_offset offset for wind_direction (in degree) if groups == "wd_class"; bins are then calculated over (wd + wd_offset) %% 360
#' @param ws_binwidth width of bins for wind velocity if groups == "ws_class"
#' @param smooth TRUE/FALSE, applies if groups = c("u_class", "v_class"); should smoothing of summary results should be performed
#' using gam_surface()?
#' @param k numeric, applies if smooth = TRUE; degree of smoothing in gam_surface()
#' @param extrapolate TRUE/FALSE, applies if smooth = TRUE; gem_smooth() returns extrapolated values for u, v coordinates that have NA for summarised y
#' if extrapolate = TRUE, those values are returned (to a certain degree depending on the value of dist)
#' @param dist numeric, fraction of 1, applies if smooth = TRUE and extrapolate = TRUE; maximum distance to coordinate-pair at which the result of 
#' gem_smooth(y) should be returned

#' 
#' @return a tibble with summarised data
#' 
#' Computed variables
#' 
#' * If groups = c("u_class", "v_class"): a tibble is returned, binned over u and v, with variables:
#' - wd: wind direction corresponding to midpoint value of u_class and v_class
#' - ws: wind velocity corresponding to midpoint value of u_class and v_class
#' - wd_class: new bins over wd considering wd_binwidth
#' - ws_class: new bins over ws considering ws_binwidth and ws_max
#' - u_class: bins over u (from input wd and ws)
#' - v_class: bins over v (from input wd and ws)
#' - y: result from fun(y, ...)
#' * If groups = NULL: groups = "wd". In this case, bins are calculated over wind direction; 
#' a tibble including wd_class and summarised y is returned
#' * groups can be strings for other varibables in data; then fun is applied over those; 
#' a tibble including groups and summarised y is returned
#' 
stat_bin_wind <- function(ws, wd, y, groups = NULL, fun = "mean", fun.args = list(), nmin = 3, ws_max = Inf, bins = 100, 
                          wd_binwidth = 45, wd_offset = 0, ws_binwidth = 1, smooth = TRUE, k = 100, extrapolate = TRUE, dist = 0.1) {
  
  data <-
    tibble::tibble(
      wd = wd,
      ws = ws,
      y = y
    ) 
  if (fun != "identity") { 
    if (is.null(groups)) groups <- "wd_class" # c("u_class", "v_class")
    ns <- function(x, ...) {sum(!is.na(x))}
    fun <- as.list(c(unlist(fun), "ns")) 
    data <- 
      data %>%
      dplyr::mutate(
        u = ws * sin(pi * wd / 180),
        v = ws * cos(pi * wd / 180)
      ) 
    uv_max <- pmin(max(abs(c(data$u, data$v)), na.rm = TRUE), ws_max)
    uv_cuts <- seq(-uv_max, uv_max, length.out = bins)
    data <-  
      data %>% 
      dplyr::mutate(
        u_class = cut(u, breaks = uv_cuts),
        v_class = cut(v, breaks = uv_cuts),
        wd_class = wd_classes(wd, wd_binwidth),
        ws_class = cut(ws, breaks = seq(0, pmin(max(ws_max, na.rm = TRUE), ws_max), ws_binwidth))
      ) %>% 
      na.omit() %>% 
      dplyr::group_by_at(groups) %>%
      dplyr::summarise_at(
        .vars = "y",
        .funs = fun,
        !!!fun.args
      ) %>%
      dplyr::ungroup() %>%
      tidyr::gather(stat, y, -groups, -ns) %>% 
      dplyr::mutate(stat = factor(stat)) %>% 
      dplyr::filter(
        ns >= nmin
      )
    if ("u_class" %in% groups & "v_class" %in% groups) {
      data <- 
        data %>% 
        dplyr::mutate(
          u = midpoints(u_class),
          v = midpoints(v_class)
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
            gam_surface(u = .$u, v = .$v, y = .$y, weights = pmin(4, .$ns) / 4,
                        k = k, extrapolate = extrapolate, dist = dist)
          }) %>% 
          dplyr::ungroup()
        data <- 
          dplyr::bind_cols(
            data_smooth,
            dplyr::select(data, -u, -v, -y, -stat)
          )
      }
      data <- 
        data %>% 
        dplyr::mutate(
          wd = uv2wd(u, v),
          ws = sqrt(u^2 + v^2),
          wd_class = cut(wd, breaks = seq(0, 360, wd_binwidth)),
          ws_class = cut(ws, breaks = seq(0, pmin(max(ws_max, na.rm = TRUE), ws_max), ws_binwidth)), 
          u_class = cut(ws * sin(pi * wd / 180), breaks = uv_cuts),
          v_class = cut(ws * cos(pi * wd / 180), breaks = uv_cuts),
          y = ifelse(ws > ws_max, NA, y)
        )
    }
  }
  
  return(data)
}
