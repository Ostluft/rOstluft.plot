#' ggplot wrapper to create a wind-based polar plot
#' 
#' @description Can mimic openair::polarplot(), i.e. wind direction / wind velocity heatmap, 
#' including options for smoothing, flexible grid setting,
#' flexible stat functions, data extrapolation and plotting over a raster map.
#' 
#' @return ggplot object
#
#' @examples 
#' require(rOstluft)
#' require(rOstluft.data)
#' require(lubridate)
#' require(ggplot2)
#' require(dplyr)
#' 
#' df <-
#'   rOstluft::read_airmo_csv(system.file("extdata", "Zch_Stampfenbachstrasse_2010-2014.csv", package = "rOstluft.data", mustWork = TRUE)) %>%
#'   rOstluft::rolf_to_openair() %>%
#'   dplyr::mutate(wday = lubridate::wday(date, label = TRUE, week_start = 1))
#' 
#' ggpolarplot(df, z = "NOx")
#' ggpolarplot(df, z = "NOx", extrapolate = FALSE, ws_max = 4, bins = 40)
#' ggpolarplot(df, z = "NOx", extrapolate = FALSE, ws_max = 3)
#' ggpolarplot(df, z = "NOx", extrapolate = TRUE, ws_max = 3)
#' ggpolarplot(df, z = "NOx", smooth = FALSE, extrapolate = FALSE, ws_max = 3, bins = 50, nmin = 10)
#' ggpolarplot(df, z = "NOx", smooth = FALSE, extrapolate = FALSE, ws_max = 4) +
#'   facet_wrap(.~wday, nrow = 2, scales = "fixed")
#' ggpolarplot(df, z = "NOx", ws_max = 4) +
#'   facet_wrap(.~wday, nrow = 2)
#' ggpolarplot(df, z = "NOx", smooth = FALSE, extrapolate = FALSE, ws_max = 4, bins = 25, nmin = 10)
#' ggpolarplot(df, z = "NOx", fun = "quantile", fun.args = list(probs = 0.95), smooth = FALSE, extrapolate = FALSE, ws_max = 4, bins = 50, nmin = 10)
#' 
ggpolarplot <- function(data,
                        z,
                        wd = "wd",
                        ws = "ws",
                        nmin = 3,
                        fun = "mean",
                        fun.args = list(na.rm = TRUE),
                        ws_max = Inf,
                        smooth = TRUE,
                        k = 100,
                        extrapolate = TRUE,
                        dist = 0.1,
                        bins = 75,
                        fill_scale = scale_fill_gradientn(colours = matlab::jet.colors(100), na.value = NA),
                        ws_unit = "m/s",
                        breaks = waiver(),
                        bg = NULL,
                        ...
) { 
  groups <- c("u_class", "v_class")
  ggplot(data, aes(wd = !!sym(wd), ws = !!sym(ws), z = !!sym(z))) +
    stat_summary_wind(
      mapping = aes(x = stat(u), y = stat(v), fill = stat(z)), 
      fun = fun, fun.args = fun.args, nmin = nmin, ws_max = ws_max, 
      smooth = smooth, k = k, extrapolate = extrapolate, dist = dist, bins = bins, groups = groups
    ) +
    geom_point(aes(x = 0, y = 0), inherit.aes = FALSE, shape = 3, color = "gray80") +
    scale_y_continuous(breaks = breaks, labels = function(ws) paste0(abs(as.numeric(ws))," ",ws_unit)) +
    scale_x_continuous(breaks = breaks) +
    guides(fill = guide_colorbar(title = z)) +
    fill_scale + 
    coord_cartpolar(bg = bg) +
    theme_polarplot
}









