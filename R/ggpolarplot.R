require(ggplot2)
require(dplyr)
require(mgcv)
require(viridis)
require(matlab)





smooth_gam_surface <- function(u, v, z, weights = NULL, k = 100, extrapolate = FALSE, force_positive = TRUE, dist = 0.05, df = NULL) {
  if (force_positive) n <- 0.5 else n <- 1
  data <- tibble(
    u = u,
    v = v,
    z = z
  )
  data$z <- data$z^n
  data$id <- 1:nrow(data)
  index <- which(!is.na(data$z))
  m <- mgcv::gam(z ~ s(u, v, k = k, bs = "gp"), 
                 data = data,
                 weights = weights,
                 method = 'REML', 
                 control =  mgcv::gam.control(nthreads = parallel::detectCores() - 1),
                 family = gaussian())
  if (extrapolate) {
    pred <- predict(m, newdata = data, type = 'response')^(1/n)
  } else {
    pred <- predict(m, type = 'response')^(1/n)
  }
  pred <- 
    tibble(
      id = as.numeric(names(pred)),
      z = pred
    )
  data <-
    data %>% 
    dplyr::select(-z) %>% 
    dplyr::left_join(pred, by = "id") %>% 
    dplyr::mutate(
      z = ifelse(mgcv::exclude.too.far(.$u, .$v, data$u[index], data$v[index], dist = dist), NA, z)
    ) %>% 
    dplyr::bind_cols(df)
    
  return(data)
}



#' kartesisch => plotted deutlich schneller als tiles in coord_polar(), braucht aber noch ein neues coord_ inkl. grid ...:
calc_polarplot <- function(data, fun.y = "mean", nmin = 3, na.rm = TRUE, ws_max = Inf, smooth = TRUE, 
                           k = 100, extrapolate = TRUE, dist = 0.1, length_out = 100, ...) {
  
  n <- function(x, ...) {sum(!is.na(x))}
  fun.y <- list(unlist(fun.y), "n")
  data <-
    tibble::tibble(
      x = data$wd,
      y = data$ws,
      fill = data$fill
    ) %>%
    dplyr::mutate(
      u = y * sin(pi * x / 180),
      v = y * cos(pi * x / 180)
    ) 
  uv_max <- pmin(max(abs(c(data$u, data$v)), na.rm = TRUE), ws_max)
  uv_cuts <- seq(-uv_max, uv_max, length.out = length_out)
  data <-  
    data %>% 
    dplyr::mutate(
      u = cut(u, breaks = uv_cuts),
      v = cut(v, breaks = uv_cuts)
     ) %>% 
    na.omit() %>%
    dplyr::group_by(u, v) %>%
    dplyr::summarise_at(
      .vars = "fill",
      .funs = fun.y,
      na.rm = na.rm
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      n >= nmin
    ) %>%
    dplyr::rename(
      fill = fun.y[[1]] # !
    ) %>%
    na.omit() %>%
    dplyr::mutate(
      u = midpoints(u),
      v = midpoints(v)
    )
  data <-
    data %>%
    tidyr::expand(u, v) %>%
    dplyr::left_join(data, by = c("u", "v"))
  if (smooth) {
    data <-
      smooth_gam_surface(u = data$u, v = data$v, z = data$fill, weights = pmin(4, data$n) / 4,
                         k = k, extrapolate = extrapolate, dist = dist) %>% 
      dplyr::rename(fill = z)
  }
  data <- 
    data %>% 
    mutate(
      # wd = uv2wd(u, v),
      ws = sqrt(u^2 + v^2),
      fill = ifelse(ws > y_cuts$y_cap, NA, fill)
    ) %>% 
    dplyr::rename(
      x = u,
      y = v
    )
  
  return(data)
}



StatWind <- ggproto("StatWind", Stat,
                    
                    compute_group = function(data, scales, fun.y = "mean", nmin = 3, ws_max = Inf, na.rm = TRUE,
                                             smooth = TRUE, k = 100, extrapolate = TRUE, dist = 0.1, length_out = 100, ...) {
                      data <- 
                        calc_polarplot(data, fun.y = fun.y, nmin = nmin, ws_max = ws_max, na.rm = na.rm,
                                       smooth = smooth, k = k, extrapolate = extrapolate, dist = dist, length_out = length_out, ...)
                      data
                    },
                    
                    required_aes = c("wd", "ws", "fill")
)



stat_wind <- function(mapping = NULL, data = NULL, geom = "raster",
                      position = "identity", na.rm = TRUE, show.legend = NA, inherit.aes = TRUE, 
                      fun.y = "mean", nmin = 1, ws_max = Inf, 
                      smooth = TRUE, k = 100, extrapolate = TRUE, dist = 0.1, length_out = 100, ...) {
  ggplot2::layer(
    stat = StatWind, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(fun.y = fun.y, nmin = nmin, na.rm = na.rm, ws_max = ws_max, 
                  smooth = smooth, k = k, extrapolate = extrapolate, dist = dist, length_out = length_out, ...)
  )
}



ggpolarplot <- function(df, 
                        z,
                        nmin = 3,
                        fun.y = "mean",
                        ws_max = 6,
                        ws_unit = "m/s",
                        smooth = TRUE,
                        k = 100,
                        extrapolate = TRUE,
                        dist = 0.1,
                        length_out = 100,
                        fill_scale = scale_fill_gradientn(colours = matlab::jet.colors(100), na.value = NA)
) { 
  
  p <-
    ggplot(df, aes(wd = wd, ws = ws, fill = !!sym(z))) +
    stat_wind(fun.y = fun.y, nmin = nmin, , ws_max = ws_max, smooth = smooth, k = k, 
              extrapolate = extrapolate, dist = dist, length_out = length_out) +
    scale_y_continuous(labels = function(ws) paste0(ws," ",ws_unit)) +
    fill_scale + 
    coord_equal() + # für kartesisch.., dann bräuchten wir aber ein eigenes 'coord_polar_cartesian()'
    theme_polarplot
  
  return(p)
}










#' Bsp:
require(rOstluft)
require(rOstluft.data)
require(lubridate)

df <-
  rOstluft::read_airmo_csv(system.file("extdata", "Zch_Stampfenbachstrasse_2010-2014.csv",package = "rOstluft.data", mustWork = TRUE)) %>%
  rOstluft::rolf_to_openair() %>% 
  dplyr::mutate(
    wday = lubridate::wday(date, label = TRUE)
  )

#' mit coord_polar, daher dauerts etwas sehr lang, daher lieber kartesisch...
p <- ggpolarplot(df, z = "NOx")
p 
p + facet_wrap(.~wday, nrow = 2)

#' inkl. background raster map und fill opacity
#' ...

#' parameter ca. wie openair::polarplot (bloss mit coord_polar, daher dauerts etwas...)
#' p <- ggpolarplot(df, z = "NOx", dist = 0.05, wd_cutwidth = 1, nmin = 1, ws_breaks = 0.125)
#' p



