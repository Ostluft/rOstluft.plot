require(dplyr)
require(tidyr)
require(ggplot2)
require(viridis)


calc_wind_stats <- function(data, fun.y = "mean", nmin = 3, wd_cutwidth = 45, ...) {
  ns <- function(x, ...) {sum(!is.na(x), ...)}
  fun.y <- as.list(c(unlist(fun.y), "ns"))
  data <- 
    tibble::tibble(
      x = data$wd,
      y = data$y
    ) %>% 
    dplyr::mutate(
      x = wd_classes(x, wd_cutwidth)
    ) 
  data <-
    data %>% 
    dplyr::group_by(x) %>%
    dplyr::summarise_at(
      .vars = "y",
      .funs = fun.y,
      na.rm = na.rm
    ) %>% 
    dplyr::ungroup() %>% 
    tidyr::gather(stat, y, -x, -ns) %>% 
    dplyr::filter(
      ns >= nmin
    ) %>% 
    dplyr::select(-ns) %>% 
    na.omit()
  
  return(data)
}



#' sneaked from here: http://www.cmap.polytechnique.fr/~lepennec/R/Radar/RadarAndParallelPlots.html
coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}



StatWind2 <- ggproto("StatWind2", Stat,
                    
                    compute_group = function(data, scales, fun.y = "mean", nmin = 3, wd_cutwidth = 45, na.rm = TRUE, ...) {
                      data <- 
                        calc_wind_stats(data, fun.y = fun.y, nmin = nmin, wd_cutwidth = wd_cutwidth, ...)
                      data
                    },
                    
                    required_aes = c("wd", "y")
)



stat_wind2 <- function(mapping = NULL, data = NULL, geom = "polygon",
                      position = "identity", na.rm = TRUE, show.legend = NA, inherit.aes = TRUE, 
                      fun.y = "mean", nmin = 1, wd_cutwidth = 45, ...) {
  ggplot2::layer(
    stat = StatWind2, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(fun.y = fun.y, nmin = nmin, na.rm = na.rm, wd_cutwidth = wd_cutwidth, ...)
  )
}



ggpradar <- function(df, 
                        y,
                        wd_cutwidth = 45,
                        nmin = 3,
                        fun.y = "mean",
                        color_scale = scale_color_viridis(discrete = TRUE),
                        fill_scale = scale_fill_viridis(discrete = TRUE, alpha = 0.25), 
                        ...
) { 
  stopifnot((360 / wd_cutwidth) %in% c(4, 8, 12, 16))
  breaks <- seq(0, 360, wd_cutwidth)
  breaks <- paste0("[", head(breaks, -1),"," ,tail(breaks, -1), ")")[seq(1, 360 / wd_cutwidth, 90 / wd_cutwidth)]
  p <-
    ggplot(df, aes(wd = wd, y = !!sym(y))) +
    stat_wind2(fun.y = fun.y, nmin = nmin, wd_cutwidth = wd_cutwidth, ...) +
    scale_x_discrete(breaks = breaks, labels = c("N", "E", "S", "W")) +
    scale_y_continuous(limits = c(0, NA), expand = c(0,0)) +
    color_scale +
    fill_scale +
    coord_radar(start = -(2 * pi / (360 / wd_cutwidth) / 2)) +
    theme_radar
  
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

#' simple
p <- ggpradar(df, "NOx")
p
#' fixed custom colour & fill
p <- ggpradar(df, "NOx", color_scale = NULL, fill_scale = NULL, color = "blue", fill = "#0000FF40")
p
#' no fills, but grouped by wday
p <- 
  ggpradar(df, "NOx",  fill_scale = NULL, fill = NA) + 
  aes(wd = wd, y = NO2, color = wday)
p
#' rather NO2
p + aes(wd = wd, y = NO2, color = wday)
#' like facets more
p + facet_wrap(.~wday) 
#' want fills with, but with non-default opacity
p <- 
  ggpradar(df, "NO2") + 
  aes(wd = wd, y = NO2, color = wday, fill = wday) + 
  scale_fill_viridis(discrete = TRUE, alpha = 0.5) + 
  facet_wrap(.~wday) 
p
#' group by different stats
p <- 
  ggpradar(df, "NO2", fun.y = list("mean", "sd")) + 
  aes(wd = wd, y = NO2, color = stat)
  #... hm, klappt noch nicht


