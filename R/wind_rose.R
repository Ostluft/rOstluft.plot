#' Plotting a windrose on a stamen terrain plot
#'
#' @param data in rolf format with WD and WVv
#' @param meta tibble with one row containing x and y coordinates of the site
#'
#' @return png image
#' @export
plt_wind_rose <- function(data, meta) {
  wide <- rOstluft::rolf_to_openair(data)
  winkel_sector <- 22.5
  wide <- dplyr::mutate(
    wide,
    wd_shifted = .data$wd - winkel_sector / 2,
    wd_class = ggplot2::cut_width(.data$wd_shifted, 22.5),
    ws_class = forcats::fct_rev(ggplot2::cut_interval(.data$ws, length = 2))
  )

 wind_rose <- ggplot(wide, aes(x = wd_class, fill = ws_class, y = stat(count / sum(count)))) +
    geom_bar(width=1, colour="grey80", size=0.5, alpha = 0.5, show.legend = TRUE) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(labels = function(x) {stringr::str_c(x*100, " %") }) +
    scale_fill_viridis_d(direction = 1)

 legend <- cowplot::get_legend(wind_rose)
 legend <- cowplot::ggdraw() + cowplot::draw_grob(legend)

 wind_rose <- wind_rose +
    coord_polar(start = -((winkel_sector/2)/360) * 2*pi) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "transparent",colour = NA),
      plot.background = element_rect(fill = "transparent",colour = NA),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      legend.position= "none"
    )

  #
  fig_wind_rose <- magick::image_graph(width = 600, height = 600, res = 72)
  print(wind_rose)
  dev.off()

  # transparency doesnt work with print, do it now
  fig_wind_rose <- magick::image_transparent(fig_wind_rose, "white", 0.01)


  fig_legend <- magick::image_graph(width = 100, height = 800, res = 72, bg = "transparent")
  print(legend)
  dev.off()

  bbox <- tibble::tibble(x = c(meta$x - 500, meta$x + 500), y = c(meta$y - 500, meta$y + 500))

  bbox <- rOstluft::transform_projection(bbox, coord = c("x", "y"),
                       initCRS = sp::CRS("+init=epsg:2056"),
                       outCRS = sp::CRS("+init=epsg:4326"))

  bbox <- c(left = bbox$x[1], right = bbox$x[2], bottom = bbox$y[1], top = bbox$y[2])

  bg <- ggmap::get_stamenmap(bbox, zoom = 16, maptype = "terrain", source = "stamen", color = "bw") %>%
    ggmap::ggmap(extent = "device", padding = 0)


  fig_bg <- magick::image_graph(width = 800, height = 800, res = 72)
  print(bg)
  dev.off()

  img <- magick::image_composite(fig_bg, fig_wind_rose, operator = "add", offset= "+100+100")
  magick::image_composite(img, fig_legend)
}
