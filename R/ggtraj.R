#' Plotting hysplit trajectory
#'
#' @param data tibble containing hysplit trajectories, format preferably similar to that of the 'openair' package
#' @param mapping ggplot mapping, typically: aes(x = lon, y = lat, group = date, color = height)
#' @param incr sequence of hours to draw an marker on the trajetory. Default -seq(24,96,24); can be NULL - then, no increment markers are plotted
#' @param lims list with xlim and ylim items defining the map section. See [ggplot::coord_quickmap()]
#' @param add_traj_labels add text labels with date and time for every trajectory
#' @param color_scale ggplot2 color scale
#'
#' @return ggplot2 object
#'
#' @examples
#' fn <- system.file("extdata", "2017_ZH-Kaserne-hysplit.rds", package = "rOstluft.data")
#' traj <- readRDS(fn)
#' traj <- dplyr::filter(traj, date < lubridate::ymd("2017-01-08"))
#' ggtraj(traj)
#'
#' @export
ggtraj <- function(data, mapping = aes(x = lon, y = lat, group = date, color = height),
                   incr = -seq(24,96,24), lims = NULL, add_traj_labels = TRUE,
                   color_scale = ggplot2::scale_color_gradient(name = "m agl.")) {

  if (is.null(lims)) {
    lims <- list(
      xlim = range(data$lon, na.rm = TRUE),
      ylim = range(data$lat, na.rm = TRUE)
    )
  }


  plot <- ggplot2::ggplot(data, mapping)

  # add background map
  plot <- plot +
    ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group), ggplot2::map_data("world"),
                          color = "gray40", fill = "gray90", inherit.aes = FALSE, size = 0.25) +
    ggplot2::coord_quickmap(xlim = lims$xlim, ylim = lims$ylim) +
    ggplot2::geom_path()


  # add increment markers along the line for every hour in incr
  if (!is.null(incr)) {
  increment_markers <- dplyr::filter(data, .data$hour.inc %in% incr)
  plot <- plot +
    # increments marker: default @ -24, -48, -72, -96
    ggplot2::geom_point(data = increment_markers, size = 1, show.legend = FALSE)
  }

  # add target markers for every site, or should we add a restrction for only one site?
  target_markers <- dplyr::filter(data, .data$hour.inc == 0)
  target_markers <- dplyr::distinct(target_markers, .data$lat, .data$lon, .keep_all = TRUE)
  plot <- plot +
    ggplot2::geom_point(data = target_markers, size = 2, shape = 21,
                        color = "white", fill = "gray20", show.legend = FALSE)


  # add origin text for every trajectory
  if (isTRUE(add_traj_labels)) {
    min_hour <- min(data$hour.inc)
    traj_origin <- dplyr::filter(data, .data$hour.inc == min_hour)
    plot <- plot +
      ggrepel::geom_text_repel(ggplot2::aes(label = format(date, "%H:00")), traj_origin,
                               color = "gray20", direction = "both", hjust = 1, vjust = 0.5,
                               nudge_x = 0.2, nudge_y = -0.1, segment.size = 0.2, size = 2) +
      ggrepel::geom_text_repel(ggplot2::aes(label = format(date, "%d.%m.%y")), traj_origin,
                               color = "gray20", direction = "both", hjust = 1, vjust = 0.5,
                               nudge_x = 0.2, nudge_y = 0.1, segment.size = 0.2, size = 2)
  }

  # add some theming, this should probably solved globally
  # should title be configurable?
  sites <- dplyr::distinct(data, .data$site)
  plot <- plot +
    theme_traj +
    ggplot2::scale_y_continuous(expand = c(0.1,0.1)) +
    ggplot2::scale_x_continuous(expand = c(0.1,0.1)) +
    color_scale

  return(plot)
}

