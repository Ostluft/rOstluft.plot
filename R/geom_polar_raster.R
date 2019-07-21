#' Background Raster for polar plots
#'
#' quick and really dirty implementation of a raster rendered on the whole extend of the panel.
#'
#' @param raster raster data to plot
#' @param interpolate interpolate Raster. see `grid::rasterGrob()`
#' @param ... unused at the moment
#'
#' @return ggplot2 layer
#' @export
#'
#'
#'
#' @examples
#' require(ggplot2)
#'
#' # prepare map as raster
#' bbox <- tibble::tibble(x = c(2683141 - 500, 2683141 + 500), y = c(1249040 - 500, 1249040 + 500))
#' bbox <- rOstluft::transform_LV95_to_WSG84(bbox)
#'
#' bbox <- c(left = bbox$lon[1], right = bbox$lon[2], bottom = bbox$lat[1], top = bbox$lat[2])
#'
#' raster_map <- ggmap::get_stamenmap(bbox, zoom = 16, maptype = "terrain",
#'                                    source = "stamen", color = "bw")
#'
#'
#' # prepare data
#' fn <- system.file("extdata", "Zch_Stampfenbachstrasse_2010-2014.csv", package = "rOstluft.data")
#' data <- rOstluft::read_airmo_csv(fn)
#'
#' wide <- rOstluft::rolf_to_openair(data)
#' winkel_sector <- 22.5
#' wide <- dplyr::mutate(
#'   wide,
#'   wd_shifted = .data$wd - winkel_sector / 2,
#'   wd_class = ggplot2::cut_width(.data$wd_shifted, 22.5),
#'   ws_class = forcats::fct_rev(ggplot2::cut_interval(.data$ws, length = 2))
#' )
#'
#' ggplot(wide, aes(x = wd_class, fill = ws_class, y = stat(count / sum(count)))) +
#'   # insert raster as background
#'   geom_polar_raster(raster_map) +
#'   # add plot
#'   geom_bar(width=1, colour="grey80", size=0.5, alpha = 0.5, show.legend = TRUE, na.rm = TRUE) +
#'   scale_x_discrete(drop = FALSE) +
#'   scale_fill_viridis_d(direction = 1) +
#'   coord_polar(start = -((winkel_sector/2)/360) * 2*pi) +
#'   theme(
#'     axis.text.x = ggplot2::element_blank()
#'   )
geom_polar_raster <- function(raster, interpolate = TRUE, ...) {
  # needs a better way to pass the data
  data <- tibble::tibble(raster = list(raster))

  layer(
    data = data,
    mapping = aes(raster = raster),
    stat = "identity",
    geom = GeomPolarRaster,
    position = "identity",
    show.legend = FALSE,
    inherit.aes = FALSE,
    params = list(
      interpolate = interpolate,
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname rOstluft.plot-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPolarRaster <- ggplot2::ggproto("GeomPolarRaster", Geom,
  required_aes = c("raster"),
  non_missing_aes = "raster",
  draw_key = ggplot2::draw_key_rect,
  draw_panel = function(data, panel_params, coord, interpolate = FALSE) {
    # needs a better way to pass the data
    grid::rasterGrob(data$raster[[1]], width = 1, height = 1, default.units = "native", interpolate = interpolate)
  }
)





