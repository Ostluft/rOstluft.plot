#' @examples 
#' # Bsp:
#' require(rOstluft)
#' require(rOstluft.data)
#' require(lubridate)
#' 
#' df <-
#'   rOstluft::read_airmo_csv(system.file("extdata", "Zch_Stampfenbachstrasse_2010-2014.csv",package = "rOstluft.data", mustWork = TRUE)) %>%
#'   rOstluft::rolf_to_openair() %>%
#'   dplyr::mutate(wday = lubridate::wday(date, label = TRUE, week_start = 1))
#' 
#' # simple
#' ggradar(df, "NOx")
#' # fixed custom colour & fill
#' ggradar(df, "NOx", color = "blue", fill = "#0000FF40")
#' # no fills, but grouped by wday including colors
#' ggradar(df, "NOx", fill = NA) +
#'   aes(x = wd_class, y = NOx, color = wday)
#' # want facets and fills with, but with custom opacity
#' ggradar(df, "NOx") +
#'   aes(x = wd_class, y = NOx, color = wday, fill = wday) +
#'   facet_wrap(.~wday) +
#'   scale_fill_viridis(discrete = TRUE, alpha = 0.5)
#' # group by different stats
#' ggradar(df, "NOx", fun = list("mean", "sd"), fun.args = list(na.rm = TRUE), fill = NA) +
#'   aes(wd = wd, y = NO2, color = stat(stat))
#' #... hm, klappt noch nicht ganz
#' @export
#' 
ggradar <- function(data,
                       z,
                        wd = "wd",
                        ws = "ws",
                        ...,
                        nmin = 3,
                        fun = "mean",
                        fun.args = list(na.rm = TRUE),
                        group = NULL,
                        ws_max = NA,
                        wd_binwidth = 45,
                        color_scale = viridis::scale_color_viridis(discrete = TRUE),
                        fill_scale = viridis::scale_fill_viridis(discrete = TRUE, alpha = 0.25),
                        bg = NULL,
                        geom = "polygon"
) { 
  
  stopifnot((360 / wd_binwidth) %in% c(4, 8, 12, 16))
  breaks <- seq(0, 360, wd_binwidth)
  breaks <- paste0("[", head(breaks, -1),"," ,tail(breaks, -1), ")")[seq(1, 360 / wd_binwidth, 90 / wd_binwidth)]
  groups <- "wd_class"
  if (is.null(group)) group <- "stat"
  smooth <- FALSE
  extrapolate <- FALSE
  
  # ?
  ggplot(data, aes(wd = !!sym(wd), ws = !!sym(ws), z = !!sym(z))) +
    stat_summary_wind(
      mapping = aes(x = stat(wd_class), y = stat(!!sym(z))), 
      ...,
      fun = fun, fun.args = fun.args, nmin = nmin, ws_max = ws_max, geom = geom, smooth = smooth, 
      extrapolate = extrapolate, wd_binwidth = wd_binwidth, wd_offset = wd_binwidth / 2, groups = groups
    ) +
    scale_x_discrete(breaks = breaks, labels = c("N", "E", "S", "W"), expand = c(0,0)) +
    scale_y_continuous(limits = c(0, NA), expand = c(0,0)) +
    color_scale +
    fill_scale +
    coord_radar(start = -2 * pi / 360 * wd_binwidth) +
    theme_radar
}















