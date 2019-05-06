stat_bin_wind <- function(ws, wd, z, groups = c("u_class", "v_class"), fun = "mean", fun.args = list(), nmin = 3, ws_max = Inf, smooth = TRUE, 
                          k = 100, extrapolate = TRUE, dist = 0.1, bins = 100) {
  
  n <- function(x, ...) {sum(!is.na(x))}
  fun <- list(unlist(fun), "n")
  data <-
    tibble::tibble(
      wd = wd,
      ws = ws,
      z = z
    ) %>%
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
      wd_class = cut(wd, breaks = seq(0, 360, length.out = bins)),
      ws_class = cut(ws, breaks = seq(0, pmin(max(ws_max, na.rm = TRUE), ws_max), length.out = bins))
    ) %>% 
    na.omit() %>% 
    dplyr::group_by_at(groups) %>%
    dplyr::summarise_at(
      .vars = "z",
      .funs = fun,
      !!!fun.args
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      n >= nmin
    ) %>%
    dplyr::rename(
      z = fun[[1]] # !
    )
  if ("u_class" %in% groups & "v_class" %in% groups) {
    data <- 
      data %>% 
      dplyr::mutate(
        u = midpoints(u_class),
        v = midpoints(v_class)
      )
    data <-
      tibble::tibble(
        u = midpoints(cut(uv_cuts, breaks = uv_cuts)),
        v = midpoints(cut(uv_cuts, breaks = uv_cuts))
      ) %>% 
      na.omit() %>% 
      tidyr::expand(u, v)  %>%
      dplyr::left_join(data, by = c("u", "v"))
    if (smooth) {
      gam_surf <- gam_surface(u = data$u, v = data$v, z = data$z, weights = pmin(4, data$n) / 4,
                              k = k, extrapolate = extrapolate, dist = dist)
      data <- dplyr::bind_cols(
        gam_surf,
        dplyr::select(data, -u, -v, -z)
      )
    }
    data <- 
      data %>% 
      dplyr::mutate(
        wd = uv2wd(u, v),
        ws = sqrt(u^2 + v^2),
        wd_class = cut(wd, breaks = seq(0, 360, length.out = bins)),
        ws_class = cut(ws, breaks = seq(0, max(ws, na.rm = TRUE), length.out = bins)),
        u_class = cut(ws * sin(pi * wd / 180), breaks = uv_cuts),
        v_class = cut(ws * cos(pi * wd / 180), breaks = uv_cuts),
        z = ifelse(ws > ws_max, NA, z)
      )
  }
  
  return(data)
}
