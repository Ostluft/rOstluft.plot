#' Wrapper around ggmap::get_stamen_map
#'
#' The functions [get_googlemap()] and [get_stamen_map()] tries to harmonize the calls to [ggmap()]. The objective is
#' to get interchangeable functions with sensible defaults. For example automatic calculation of the zoom. The function
#' [bbox_lv95()] generates a bbox object compatible with both functions.
#'
#' @param bbox list with items `left`, `bottom`, `right` and `top` as WSG84 coordinates. Additional `width`
#'   in meters if `zoom = "auto"`. Use [bbox_lv95()] to create a compatible bounding box list
#' @param width of resulting map in pixels.
#' @param zoom zoomlevel from 0 to 18 or "auto"
#' @param color color or black-and-white. Changed default to "bw"
#' @param ... forwarded to [ggmap::get_googlemap()]
#'
#' @return a ggmap object (a classed raster object with a bounding box attribute)
#' @export
#'
#' @examples
#' bb <- bbox_lv95(2683141, 1249040, 500) # site Zch_Stamfenbachstrasse
#' get_stamen_map(bb) %>% ggmap::ggmap()
get_stamen_map <- function(bbox, width = 640, zoom = "auto", color =  "bw", ...) {

  if (zoom == "auto") {
    zoom <- auto_zoom(bbox$width, width, bbox$center["lat"])
  }

  zoom <- max(0, min(zoom, 18))
  bbox_osm <- c(bbox[["left"]], bbox[["bottom"]], bbox[["right"]], bbox[["top"]])
  ggmap::get_stamenmap(bbox_osm, zoom = zoom, color = color, ...)
}


#' Wrapper around ggmap::get_googlemap
#'
#' The functions [get_googlemap()] and [get_stamen_map()] tries to harmonize the calls to [ggmap()]. The objective is
#' to get interchangeable functions with sensible defaults. For example automatic calculation of the zoom. The function
#' [bbox_lv95()] generates a bbox object compatible with both functions. Before using this function you need to register
#' your google API key with [ggmap::register_google(key)]. For details consult the documentation of the
#' [Maps Static API](https://developers.google.com/maps/documentation/maps-static/intro) and [ggmap::get_googlemap()].
#'
#'
#' @param bbox list with item `center = (lon, lat)` as WSG84 coordinates. Additional `width` in meters if `zoom = "auto"`.
#'   Use [bbox_lv95()] to create a compatible bounding box list
#' @param width of resulting map in pixels. Is divided trough scale to calculate size.
#' @param zoom of tile map as Integer or "auto"
#' @param scale affects the size of labels. [ggmap::get_googlemap()] multiplies the size with this factor.
#' @param language character string providing language of map labels (for themes with them) in the format "en-EN".
#'   not all languages are supported; for those which aren't the default language is used.
#' @param color color or black-and-white. Changed default to "bw"
#' @param style character string or named vector to style the map. This is a powerful and complex specification. See
#'  [Styled Maps](https://developers.google.com/maps/documentation/maps-static/styling#style-syntax) in the documentation of
#'  the google maps platform
#' @param ... forwarded to [ggmap::get_googlemap()]
#'
#' @return a ggmap object (a classed raster object with a bounding box attribute)
#' @export
#'
#' @examples
#' ggmap::register_google(Sys.getenv("GGMAP_GOOGLE_API_KEY"))
#' bb <- bbox_lv95(2683141, 1249040, 500) # site Zch_Stamfenbachstrasse
#' get_google_map(bb) %>%  ggmap::ggmap()
get_google_map <- function(bbox, width = 640, zoom = "auto", scale = 2, language = "de-CH", color = "bw",
                           style = c(feature = "all", element = "labels", visibility = "off"), ...) {
  width <- width / scale

  if (zoom == "auto") {
    zoom <- auto_zoom(bbox[["width"]], width, bbox[["center"]][["lat"]])
  }

  dots <- rlang::dots_list(...)

  if (rlang::has_name(dots, "size")) {
    size <- dots[["size"]]
    dots[["size"]] <- NULL
  } else {
    size = c(width, width)
  }

  zoom <- max(3, min(zoom, 22)) # valids zoom see [ggmap::get_googlemap()]

  rlang::exec(ggmap::get_googlemap, bbox$center, zoom = zoom, size = size, scale = scale,
              language = language, color = color, style = style, !!!dots)
}

#' bbox in WSG84 from LV95
#'
#' This Function creates a bbox for usage with [get_stamen_map()] or [get_google_map()]. Exactly one set of
#' (x, y, r) or (x, y, w, h) or (x1, y1, x2, y2) must be supplied as arguments. The actual position, width and height
#' in the resulting map can slightly differ on behalf of the projections from LV95 (EPSG:2056) to WSG84 (EPSG:4326) to
#' Pseudo Mercator (EPSG:3857). But should be negligible in most cases.
#'
#' @param x LV95 x coordinate of center in m
#' @param y LV95 y coordinate of center in m
#' @param r radius around center in m (defines a quadrat)
#' @param w width of box in m (defines a rectancle)
#' @param h height of box in m (defines a rectancle)
#' @param x1 left side (lower left point together with y1) in m
#' @param y1 bottom side (lower left point together with x1) in m
#' @param x2 right side (upper right point together with x2) in m
#' @param y2 top side (upper right point together with y2) in m
#'
#' @return bbox as list with left, right, bottom, top, center = (lon, lat) as WSG84 coordinates, width and height in m
#' @export
#'
#' @examples
#' # use in this form for background maps in polar plots
#' bbox_lv95(2683141, 1249040, 500)
#'
#' # specific aspect ratio
#' bbox_lv95(2683141, 1249040, w = 700, h = 0.66 * 700)
#'
#' # from a bounding box in LV95
#' bbox_lv95(x1 = 2683000, y1 = 1249000, x2 = 2683700, y2 = 1249500)
bbox_lv95 <- function(x = NA, y = NA, r = NA, w = NA, h = NA, x1 = NA, y1 = NA, x2 = NA, y2 = NA) {
  xyr <- c(!is.na(c(x, y, r)),  is.na(c(w, h, x1, y1, x2, y2)))
  xywh <- c(!is.na(c(x, y, w, h)),  is.na(c(r, x1, y1, x2, y2)))
  xxyy <- c(!is.na(c(x1, y1, x2, y2)), is.na(c(x, y, w, h, r)))
  bbox <- NULL

  if (all(xyr)) {
    bbox <- tibble::tibble(x = c(x - r, x + r, x), y = c(y - r, y + r, y))
  }

  if (all(xywh)) {
    bbox <- tibble::tibble(x = c(x - w/2, x + w/2, x), y = c(y - h/2, y + h/2, y))
  }


  if (all(xxyy)) {
    if ((x2 > x1) | (y2 > y1)) {
      bbox <- tibble::tibble(x = c(x1, x2, (x2 - x1) / 2 ), y = c(y1, y2, (y2 - y1) / 2))
    } else {
      stop("x1,y1 is greater than x2,y2")
    }
  }

  if (is.null(bbox)) {
    stop("Invalid arguments: exactly one set of (x, y, r) or (x, y, w, h) or (x1, y1, x2, y2) must be supplied")
  }


  bbox <- rOstluft::transform_LV95_to_WSG84(bbox)
  list(
    left = bbox$lon[1], right = bbox$lon[2],
    bottom = bbox$lat[1], top = bbox$lat[2],
    center = c(lon = bbox$lon[3], lat = bbox$lat[3]),
    width = bbox$x[2] - bbox$x[1], height = bbox$y[2] - bbox$y[1]
  )
}



# Source: https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Resolution_and_Scale
#
# the resolution (distance / pixel) on a specific latitude is:
#   res = distance / pixels = earth_circumference / pixel_per_tile / 2 ^ zoom * cos(lat)
#   2 ^ zoom = pixels / distance * earth_circumference / pixel_per_tile * cos(lat)
auto_zoom <- function(distance, pixels, lat, pixel_per_tile = 256) {
  earth_circumference <- 40075016.686
  floor(log2(pixels / distance * earth_circumference / pixel_per_tile * cos(lat * pi / 180) ))
}
