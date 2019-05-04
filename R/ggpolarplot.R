require(ggplot2)
require(dplyr)
require(mgcv)
require(viridis)
require(matlab)





smooth_gam_surface <- function(u, v, z, weights = NULL, k = 100, interpolate = FALSE, force_positive = TRUE, dist = 0.05, df = NULL) {
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
  if (interpolate) {
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



calc_polarplot <- function(data, fun.y = "mean", nmin = 3, wd_cutwidth = 5.625, ws_breaks = 0.25, na.rm = TRUE,
                           y_cuts = list(nclass = 4, y_boundary = 0, y_cap = Inf, dig_lab = 1), smooth = TRUE, k = 100, interpolate = TRUE, dist = 0.1, ...) {
  # length_out <- 61
  n <- function(x, ...) {sum(!is.na(x), ...)}
  fun.y <- list(unlist(fun.y), "n")
  data <- 
    tibble::tibble(
      x = data$wd,
      y = data$ws,
      fill = data$fill
    ) %>% 
    dplyr::mutate(
      x = wd_classes(x, wd_cutwidth),
      y = cut_fun(pmin(y, 1.01 * y_cuts$y_cap), y_cuts = y_cuts)
      # x = cut(x, breaks = seq(0, 360, length.out = length_out)),
      # y = cut(pmin(y, 1.01 * y_cuts$y_cap), breaks = seq(0, pmin(max(y, na.rm = TRUE),  y_cuts$y_cap), length.out = length_out))
    ) 
  data <-
    data %>% 
    dplyr::group_by(x, y) %>%
    dplyr::summarise_at(
      .vars = "fill",
      .funs = fun.y,
      na.rm = na.rm,
      ...
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
      # x = (as.numeric(x)  - 1) * length_out + length_out / 2,
      # y = (as.numeric(y) - 1) * length_out + length_out / 2
      x = (as.numeric(x)  - 1) * wd_cutwidth + wd_cutwidth / 2,
      y = (as.numeric(y) - 1) * ws_breaks + ws_breaks / 2
    )
  data <-
    data %>% 
    tidyr::expand(x, y) %>%
    dplyr::left_join(data, by = c("x", "y")) %>%
    dplyr::mutate(
      u = y * sin(pi * x / 180),
      v = y * cos(pi * x / 180)
    )
  if (smooth) {
    data <- 
      smooth_gam_surface(u = data$u, v = data$v, z = data$fill,weights = pmin(4, data$n) / 4, 
                         k = k, interpolate = interpolate, dist = dist, df = dplyr::select(data, x, y)) %>% 
      dplyr::rename(
        fill = z
      )
  }

  return(data)
}



#' #' oder doch kartesisch? => plotted deutlich schneller, braucht aber noch ein neues coord_ inkl. grid ...:
#' calc_polarplot <- function(data, fun.y = "mean", nmin = 3, wd_cutwidth = 5.625, ws_breaks = 0.25, na.rm = TRUE,
#'                            y_cuts = list(nclass = 4, y_boundary = 0, y_cap = Inf, dig_lab = 1), smooth = TRUE, k = 100, interpolate = TRUE, dist = 0.1, ...) {
#'   length_out <- 61
#'   n <- function(x, ...) {sum(!is.na(x), ...)}
#'   fun.y <- list(unlist(fun.y), "n")
#'   data <- 
#'     tibble::tibble(
#'       x = data$wd,
#'       y = data$ws,
#'       fill = data$fill
#'     ) %>% 
#'     dplyr::mutate(
#'       u = y * sin(pi * x / 180),
#'       v = y * cos(pi * x / 180),
#'       u = cut(pmin(u, 1.01 * y_cuts$y_cap), breaks = seq(-pmin(max(abs(u), na.rm = TRUE),  y_cuts$y_cap), pmin(max(abs(u), na.rm = TRUE),  y_cuts$y_cap), length.out = length_out)),
#'       v = cut(pmin(v, 1.01 * y_cuts$y_cap), breaks = seq(-pmin(max(abs(v), na.rm = TRUE),  y_cuts$y_cap), pmin(max(abs(v), na.rm = TRUE),  y_cuts$y_cap), length.out = length_out)),
#'      ) 
#'   data <-
#'     data %>% 
#'     dplyr::group_by(u, v) %>%
#'     dplyr::summarise_at(
#'       .vars = "fill",
#'       .funs = fun.y,
#'       na.rm = na.rm,
#'       ...
#'     ) %>% 
#'     dplyr::ungroup() %>% 
#'     dplyr::filter(
#'       n >= nmin
#'     ) %>% 
#'     dplyr::rename(
#'       fill = fun.y[[1]] # !
#'     ) %>% 
#'     na.omit() %>% 
#'     dplyr::mutate(
#'       u = as.numeric(u),
#'       v = as.numeric(v)
#'     )
#'   data <-
#'     data %>% 
#'     tidyr::expand(u, v) %>%
#'     dplyr::left_join(data, by = c("u", "v"))
#'   if (smooth) {
#'     data <- 
#'       smooth_gam_surface(u = data$u, v = data$v, z = data$fill, weights = pmin(4, data$n) / 4, 
#'                          k = k, interpolate = interpolate, dist = dist) %>% 
#'       dplyr::rename(
#'         x = u,
#'         y = v,
#'         fill = z
#'       )
#'   }
#'   
#'   return(data)
#' }




StatWind <- ggproto("StatWind", Stat,
                    
                    compute_group = function(data, scales, fun.y = "mean", nmin = 3, wd_cutwidth = 45, ws_breaks = 0.25, na.rm = TRUE,
                                             y_cuts = list(nclass = 4, y_boundary = 0, y_cap = Inf, dig_lab = 1), smooth = TRUE, k = 100, interpolate = TRUE, dist = 0.1, ...) {
                      data <- 
                        calc_polarplot(data, fun.y = fun.y, nmin = nmin, wd_cutwidth = wd_cutwidth, ws_breaks = ws_breaks, na.rm = na.rm,
                                       y_cuts = y_cuts, smooth = smooth, k = k, interpolate = interpolate, dist = dist, ...)
                      data
                    },
                    
                    required_aes = c("wd", "ws", "fill")
)




stat_wind <- function(mapping = NULL, data = NULL, geom = "tile",
                      position = "identity", na.rm = TRUE, show.legend = NA, inherit.aes = TRUE, 
                      fun.y = "mean", nmin = 1, wd_cutwidth = 5.625, ws_breaks = 0.25,
                      y_cuts = list(nclass = 4, y_boundary = 0, y_cap = Inf, dig_lab = 1), 
                      smooth = TRUE, k = 100, interpolate = TRUE, dist = 0.1, ...) {
  ggplot2::layer(
    stat = StatWind, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(fun.y = fun.y, nmin = nmin, na.rm = na.rm, wd_cutwidth = wd_cutwidth, ws_breaks = ws_breaks, 
                  y_cuts = y_cuts, smooth = smooth, k = k, interpolate = interpolate, dist = dist, ...)
  )
}



ggpolarplot <- function(df, 
                        z,
                        wd_cutwidth = 5,
                        nmin = 3,
                        fun.y = "mean",
                        ws_max = 6,
                        ws_breaks = 0.125,
                        ws_unit = "m/s",
                        smooth = TRUE,
                        k = 100,
                        interpolate = TRUE,
                        dist = 0.1,
                        fill_scale = scale_fill_gradientn(colours = matlab::jet.colors(100), na.value = NA),
                        expand = c(0, 0)
) { 
  
  y_cuts <- list(breaks = seq(0, ws_max, ws_breaks), y_boundary = 0, y_cap = ws_max, dig_lab = 1)
  p <-
    ggplot(df, aes(wd = wd, ws = ws, fill = !!sym(z))) +
    stat_wind(fun.y = fun.y, nmin = nmin, wd_cutwidth = wd_cutwidth, ws_breaks = ws_breaks, 
              y_cuts = y_cuts, smooth = smooth, k = k, interpolate = interpolate, dist = dist) +
    scale_x_continuous(breaks = seq(0, 270, 90) + wd_cutwidth / 2, labels = c("N", "E", "S", "W")) +
    scale_y_continuous(expand = expand, labels = function(ws) paste0(ws," ",ws_unit)) +
    fill_scale + 
    # coord_equal() + # für kartesisch.., dann bräuchten wir aber ein eigenes 'coord_polar_cartesian()'
    coord_polar(start = -(wd_cutwidth / 360 / 2 * 2 * pi)) + # hier auskommentieren zum testen von kartesisch
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

#' mit coord_polar, daher dauerts etwas...
p <- ggpolarplot(df, z = "NOx")
p 
p + facet_wrap(.~wday, nrow = 2)

#' inkl. background raster map und fill opacity
#' ...

#' parameter ca. wie openair::polarplot (bloss mit coord_polar, daher dauerts etwas...)
#' p <- ggpolarplot(df, z = "NOx", dist = 0.05, wd_cutwidth = 1, nmin = 1, ws_breaks = 0.125)
#' p



