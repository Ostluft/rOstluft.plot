#' Plotting wind density
#'
#' Quick and dirty demonstration of concept. Needs some work
#'
#' @param data in rolf format containing WD and WVv in h1 Interval
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' h1 <- system.file("extdata", "Zch_Stampfenbachstrasse_h1_2013_Jan.csv",
#'                   package = "rOstluft.data", mustWork = TRUE)
#'
#' data <- rOstluft::read_airmo_csv(h1)
#' plt_wind_density(data)
plt_wind_density <- function(data) {
  wide <- rOstluft::rolf_to_openair(data)
  wide <- dplyr::mutate(wide, hour = lubridate::hour(.data$date))

  density <- ggplot(wide, aes(wd, hour)) +
    stat_density_2d(aes(fill = stat(density)), geom = "raster", contour = FALSE,
                    show.legend = FALSE, n = 200, na.rm = TRUE) +
    scale_fill_viridis_c() +
    geom_density2d(color = "black", na.rm = TRUE) +
    coord_cartesian(ylim = c(-0.5, 23.5),  expand = FALSE)


  box <- ggplot(wide, aes(x = hour, y = ws, group = hour)) +
    geom_boxplot(outlier.alpha = 0.1, na.rm = TRUE) +
    coord_flip(xlim = c(-0.5, 23.5), expand = FALSE) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())

  cowplot::plot_grid(density, box)
}



