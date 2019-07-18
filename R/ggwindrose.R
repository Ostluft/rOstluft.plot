ggwindrose <- function(data,
                       mapping,
                       ...,
                       wd_binwidth = 45,
                       ws_max = NA,
                       fill_scale = viridis::scale_fill_viridis(discrete = TRUE),
                       bg = NULL,
                       wd_cutfun = NULL,
                       ws_cutfun = NULL
) {

  if (is.null(wd_cutfun)) wd_cutfun <- function(wd) wd_classes(wd, wd_binwidth = wd_binwidth)
  if (is.null(ws_cutfun)) ws_cutfun <- function(ws) ws_classes(ws, ws_max = ws_max)
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
    scale_x_continuous(breaks = c(0, 90, 180, 270) / wd_binwidth + 1, labels = c("N", "E", "S", "W"), expand = c(0,0)) +
    scale_y_continuous(limits = c(0, NA), expand = c(0,0)) +
    fill_scale +
    guides(fill = guide_legend(title = rlang::quo_text(mapping$z))) +
    theme_windrose

  return(plot)
}