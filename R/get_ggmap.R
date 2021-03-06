#' Wrapper to load a ggmap
#'
#' @param coord a data.frame or tibble with coordinates (latlon) defining the map extensions in the form of coord = tibble(lon = ..., lat = ..., type = c("center", "lb", "rt", ...));
#' the reason for this structure is, that [ggmap::get_googlemap()] requires a different way (center + zoom) to specify the map frame than [ggmap::get_stamenmap()] (bounding box). If needed, coord is
#' converted to bounding box by [rOstluft.plot::bbox()] which takes type == "lb" as bottom & left coordinate, and "rt" as top & right, respectively.
#' @param googlekey your google api key as a character string (see ?[ggmap::get_googlemap()]; then, a google map is downloaded) or, if NA, a map via  [ggmap::get_stamenmap()] is returned
#' @param zoom zoom factor for [ggmap::get_googlemap()]
#' @param mapstyle style parameter for [ggmap::get_googlemap()]
#' @param maptype parameter for [ggmap::get_googlemap()]
#' @param mapcolor color parameter for [ggmap::get_googlemap()]
#' @param ... additional parameter for [ggmap::get_googlemap()] or [ggmap::get_stamenmap()], respectively
#'
#' @return ggplot2 / ggmap object
#' @export
get_ggmap <- function(coord,
                      googlekey = NA,
                      zoom = 12,
                      mapstyle = c(feature = "all", element = "labels", visibility = "off"),
                      maptype = "terrain",
                      mapcolor = "bw",
                      ...) {
  if (is.na(googlekey)) {
    map <- ggmap::get_stamenmap(bbox = bbox(coord), ...)
  } else {
    ggmap::register_google(key = googlekey)
    map <- ggmap::get_googlemap(center = c(lon = coord$lon[coord$type == "center"], lat = coord$lat[coord$type == "center"]),
                                zoom = zoom, maptype = maptype, color = mapcolor, language = "de-CH", style = mapstyle, ...)
  }

  return(map)
}
