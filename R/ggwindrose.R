ggwindrose <- function(data,
                       mapping,
                       ...,
                       wd_cutfun = function(wd) wd_classes(wd, wd_binwidth = 45),
                       wd_binwidth = 45, # still needed for breaks ..
                       ws_cutfun = function(ws) ws_classes(ws, ws_max = 6),
                       fill_scale = viridis::scale_fill_viridis(discrete = TRUE, direction = -1),
                       bg = NULL
) {

  breaks <- seq(wd_binwidth, 360, wd_binwidth) / wd_binwidth
  breaks <- breaks[c(90, 180, 270, 360) / wd_binwidth] - 1
  # breaks <- seq(0, 360, wd_binwidth)
  # breaks <- paste0("[", head(breaks, -1),"," ,tail(breaks, -1), ")")[seq(1, 360 / wd_binwidth, 90 / wd_binwidth)]

  plot <-
    ggplot(data, mapping) +
    stat_summary_wind(
      mapping = aes(x = as.numeric(stat(wd)), y = stat(n), group = stat(ws), fill = stat(ws)),
      ...,
      geom = "bar", wd_cutfun = wd_cutfun,
      wd_offset = wd_binwidth / 2, ws_cutfun = ws_cutfun, groups = c("wd", "ws"),
      position = "stack", color = "white", width = 1, size = 0.25
    ) +
    coord_polar2(start = -2 * pi / 360 * wd_binwidth / 2, bg = bg) +
    scale_x_continuous(breaks = breaks, labels = c("N", "E", "S", "W"), expand = c(0,0)) +
    scale_y_continuous(limits = c(0, NA), expand = c(0,0)) +
    fill_scale +
    theme_windrose

  return(plot)
}
