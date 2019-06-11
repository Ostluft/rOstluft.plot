#' Summarise and ggplot data by wind direction bins
#'
#' @examples
#' require(rOstluft)
#' require(rOstluft.data)
#' require(rOstluft.plot)
#' require(lubridate)
#' require(ggplot2)
#'
#' df <-
#'   rOstluft::read_airmo_csv(system.file("extdata", "Zch_Stampfenbachstrasse_2010-2014.csv",package = "rOstluft.data", mustWork = TRUE)) %>%
#'   rOstluft::rolf_to_openair() %>%
#'   dplyr::mutate(wday = lubridate::wday(date, label = TRUE, week_start = 1))
#'
#' ggradar(df, aes(wd = wd, ws = ws, z = NOx), fill = "blue", color = "blue", alpha = 0.5) + ylab("NOx")
#'
#' q95 <- function(x, ...) quantile(x, 0.95, ...)
#' ggradar(df, aes(wd = wd, ws = ws, z = NOx, group = stat(stat), color = stat(stat), fill = stat(stat)), fill = NA, fun = list(a = "mean", "median", "q95")) + ylab("NOx")
#'
#' df %>%
#'   dplyr::select(wd, ws, NO, NOx, wday) %>%
#'   tidyr::gather(par, val, -wd, -ws, -wday) %>%
#'   ggradar(aes(wd = wd, ws = ws, z = val, group = par, fill = par, color = par)) + ylab("mean") +
#'   facet_wrap(wday~.)
#'
#' @export
ggradar <- function(data,
                    mapping,
                    ...,
                    nmin = 3,
                    fun = "mean",
                    fun.args = list(na.rm = TRUE),
                    ws_max = NA,
                    wd_cutfun = function(wd) wd_classes(wd, wd_binwidth = 45),
                    wd_binwidth = 45, # still needed for coord_radar and breaks ..
                    color_scale = viridis::scale_color_viridis(discrete = TRUE),
                    fill_scale = viridis::scale_fill_viridis(discrete = TRUE, alpha = 0.25),
                    geom = "polygon"
) {

  breaks <- seq(0, 360, wd_binwidth)
  breaks <- paste0("[", head(breaks, -1),"," ,tail(breaks, -1), ")")[seq(1, 360 / wd_binwidth, 90 / wd_binwidth)]

  plot <-
    ggplot(data, mapping) +
    stat_summary_wind(
      mapping = aes(x = stat(wd), y = stat(z)),
      ...,
      fun = fun, fun.args = fun.args, nmin = nmin, ws_max = ws_max, geom = geom, wd_cutfun = wd_cutfun,
      wd_offset = wd_binwidth / 2, ws_cutfun = ws_classes, groups = NULL
    ) +
    coord_radar(start = -2 * pi / 360 * wd_binwidth / 2) +
    scale_x_discrete(breaks = breaks, labels = c("N", "E", "S", "W")) +
    scale_y_continuous(limits = c(0, NA), expand = c(0,0)) +
    color_scale +
    fill_scale +
    theme_radar

  return(plot)
}















