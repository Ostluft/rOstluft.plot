#' @name rOstluft.plot-defunct
#' @title Defunct Functions in Package rOstluft.plot
#' @rdname rOstluft.plot-defunct
#' @details `get_stamen_map()` is replaced by `get_stadia_map()`
#'
#' The functions [get_googlemap()] and [get_stamen_map()] tries to harmonize the calls to [ggmap()]. The objective is
#' to get interchangeable functions with sensible defaults. For example automatic calculation of the zoom. The function
#' [bbox_lv95()] generates a bbox object compatible with both functions.
#'
#' @section Attribution:
#' * Toner and Terrain: Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under ODbL.
#' * Watercolor: Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under CC BY SA.
#'
#' @param bbox list with items `left`, `bottom`, `right` and `top` as WSG84 coordinates. Additional `width`
#'   in meters if `zoom = "auto"`. Use [bbox_lv95()] to create a compatible bounding box list
#' @param width of resulting map in pixels.
#' @param zoom zoomlevel from 0 to 18 or "auto"
#' @param color color or black-and-white. Changed default to "bw"
#' @param ... forwarded
#'
#' @return a ggmap object (a classed raster object with a bounding box attribute)
#' @export
get_stamen_map <- function(bbox, width = 640, zoom = "auto", color =  "bw", ...) {
  .Defunct("Please use get_stadia_map()")

  zoom <- max(0, min(zoom, 18))
  bbox_osm <- c(bbox[["left"]], bbox[["bottom"]], bbox[["right"]], bbox[["top"]])
  ggmap::get_stamenmap(bbox_osm, zoom = zoom, color = color, ...)
}
