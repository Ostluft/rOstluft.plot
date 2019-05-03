require(ggplot2)
require(dplyr)
require(mgcv)
require(viridis)
require(matlab)





wd_classes <- function(wd, wd_cutwidth = 45, ...) { # in helpers verschieben
  stopifnot()
  if ((360 / wd_cutwidth) %in% c(4, 8, 12, 16)) {
    wd <- (wd + wd_cutwidth / 2) %% 360
  }
  wd <- ggplot2::cut_width(wd, width = wd_cutwidth, closed = "left", boundary = 0, ...)
  return(wd)
}


cut_fun <- function(y, y_cuts, ...) { # in helpers verschieben
  if (!is.null(y_cuts[[1]])) {
    y <- 
      switch(names(y_cuts)[1],
             nclass = ggplot2::cut_interval(y, n = y_cuts$nclass, dig.lab = y_cuts$dig_lab, ...),
             cutwidth = ggplot2::cut_width(y, width = y_cuts$cutwidth, boundary = y_cuts$y_boundary, ...),
             breaks = base::cut(y, breaks = y_cuts$breaks, dig.lab = y_cuts$dig_lab,...)
      )
  }
  return(y)
}


smooth_fun <- function(data, k = 100, interpolate = FALSE, force_positive = TRUE, dist = 0.05) {
  if (force_positive) n <- 0.5 else n <- 1
  data$fill <- data$fill^n
  data$id <- 1:nrow(data)
  index <- which(!is.na(data$fill))
  m <- mgcv::gam(fill ~ s(u, v, k = k, bs = "gp"), 
                 data = data,
                 weights = pmin(4, data$n) / 4,
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
      fill = pred
    )
  data <-
    data %>% 
    dplyr::select(-fill) %>% 
    dplyr::left_join(pred, by = "id") %>% 
    dplyr::mutate(
      fill = ifelse(mgcv::exclude.too.far(.$u, .$v, data$u[index], data$v[index], dist = dist), NA, fill)
    )
    
  return(data)
}



calc_polarplot <- function(data, fun.y = "mean", nmin = 3, wd_cutwidth = 5.625, ws_breaks = 0.25, na.rm = TRUE,
                           y_cuts = list(nclass = 4, y_boundary = 0, y_cap = Inf, dig_lab = 1), smooth = TRUE, k = 100, interpolate = TRUE, dist = 0.1, ...) {
  n <- function(x, ...) {sum(!is.na(x), ...)}
  fun.y <- list(unlist(fun.y), "n")
  data <- 
    tibble::tibble(
      x = wd_classes(data$wd, wd_cutwidth),
      y = cut_fun(pmin(data$ws, 1.01 * y_cuts$y_cap), y_cuts = y_cuts),
      fill = data$fill
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
      x = (as.numeric(x)  - 1) * wd_cutwidth + wd_cutwidth / 2, 
      y = (as.numeric(y) - 1) * ws_breaks + ws_breaks / 2
    )
  data <-
    data %>% 
    tidyr::expand(x, y) %>% 
    left_join(data, by = c("x", "y")) %>% 
    dplyr::mutate(
      u = y * sin(pi * x / 180),
      v = y * cos(pi * x / 180)
    ) 
  if (smooth) {
    data <- smooth_fun(data, k = k, interpolate = interpolate, dist = dist)
  }
  
  # # ! oder doch kartesisch (coord_polar braucht lange zum transformieren...)?
  # data <-
  #   data %>%
  #   dplyr::select(-x, -y) %>%
  #   dplyr::rename(
  #     x = u,
  #     y = v
  #   )
  
  return(data)
}




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
    coord_polar(start = -(wd_cutwidth / 360 / 2 * 2 * pi)) + 
    theme_minimal() +
    theme(
      axis.ticks.y = element_line(),
      panel.ontop = TRUE,
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linetype = 2, color = "gray80")
    )
  
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

p <- ggpolarplot(df, z = "NOx")
p
p + facet_wrap(.~wday, nrow = 2)

#' #' ca. wie openair::polarplot (bloss mit coord_polar, daher dauerts etwas...)
#' p <- ggpolarplot(df, z = "NOx", dist = 0.05, wd_cutwidth = 1, nmin = 1, ws_breaks = 0.125, expand = c(0,0))
#' p

#' #' oder auch offline
#' data <-
#'   df %>%
#'   dplyr::rename(
#'     fill = NOx
#'   ) %>%
#'   calc_polarplot()
#' p <-
#'   ggplot(data, aes(x = x, y = y, fill = fill)) +
#'   geom_tile() +
#'   scale_y_continuous(expand = c(0.1,0)) +
#'   fill_scale +
#'   coord_polar(start = -offset)
#' p



