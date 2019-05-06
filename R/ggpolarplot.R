
ggpolarplot <- function(data,
                        z,
                        wd = "wd",
                        ws = "ws",
                        nmin = 3,
                        fun = "mean",
                        fun.args = list(na.rm = TRUE),
                        ws_max = 6,
                        ws_unit = "m/s",
                        smooth = TRUE,
                        k = 100,
                        extrapolate = TRUE,
                        dist = 0.1,
                        bins = 75,
                        fill_scale = scale_fill_gradientn(colours = matlab::jet.colors(100), na.value = NA),
                        ...
) { 
  
  ggplot(df, aes(wd = !!sym(wd), ws = !!sym(ws), z = !!sym(z))) +
    stat_summary_wind(
      mapping = aes(x = stat(u), y = stat(v), fill = stat(z)), 
      fun = fun, fun.args = fun.args, nmin = nmin, ws_max = ws_max, 
      smooth = smooth, k = k, extrapolate = extrapolate, dist = dist, bins = bins
    ) +
    scale_y_continuous(labels = function(ws) paste0(ws," ",ws_unit)) +
    guides(fill = guide_colorbar(title = z)) +
    fill_scale + 
    coord_equal() + # für kartesisch.., dann bräuchten wir aber ein eigenes 'coord_polar_cartesian()'
    theme_polarplot
}




#' Bsp:
require(rOstluft)
require(rOstluft.data)
require(lubridate)
require(ggplot2)
require(dplyr)

df <-
  rOstluft::read_airmo_csv(system.file("extdata", "Zch_Stampfenbachstrasse_2010-2014.csv", package = "rOstluft.data", mustWork = TRUE)) %>%
  rOstluft::rolf_to_openair() %>% 
  dplyr::mutate(wday = lubridate::wday(date, label = TRUE, week_start = 1))

ggpolarplot(df, z = "NOx")
ggpolarplot(df, z = "NOx", extrapolate = FALSE, ws_max = 4, bins = 40)
ggpolarplot(df, z = "NOx", extrapolate = FALSE, ws_max = 3)
ggpolarplot(df, z = "NOx", extrapolate = TRUE, ws_max = 3)
ggpolarplot(df, z = "NOx", smooth = FALSE, extrapolate = FALSE, ws_max = 3, bins = 50, nmin = 10)
ggpolarplot(df, z = "NOx", smooth = FALSE, extrapolate = FALSE, ws_max = 4) + 
  facet_wrap(.~wday, nrow = 2)
ggpolarplot(df, z = "NOx", ws_max = 4) + 
  facet_wrap(.~wday, nrow = 2)
ggpolarplot(df, z = "NOx", smooth = FALSE, extrapolate = FALSE, ws_max = 4, bins = 25, nmin = 10)
ggpolarplot(df, z = "NOx", fun = "quantile", fun.args = list(probs = 0.95), smooth = FALSE, extrapolate = FALSE, ws_max = 4, bins = 50, nmin = 10)
#' inkl. background raster map und fill opacity
#' ...





