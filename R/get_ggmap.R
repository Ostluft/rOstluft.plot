get_ggmap <- function(coord, 
                      googlekey = NA, 
                      zoom = 12, 
                      shape = 3, 
                      color = "gray20", 
                      scale = 0.8,
                      midpoint = FALSE,
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
  map <-
    map %>% 
    ggmap::ggmap() +
    geom_point(data = coord, aes(x = lon, y = lat), shape = shape, color = color) +
    theme_void()
  
  return(map)
}