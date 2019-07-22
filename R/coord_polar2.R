#' Customized coord_polar
#'
#' Customizing needs to be done
#'
#' @param bg raster for background image
#' @inheritParams ggplot2::coord_polar
#'
#' @seealso `ggplot2::cord_polar()`
#'
#' @return ggplot2 coord system
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' fn <- system.file("extdata", "Zch_Stampfenbachstrasse_2010-2014.csv", package = "rOstluft.data")
#' data <- rOstluft::read_airmo_csv(fn)
#'
#' wide <- rOstluft::rolf_to_openair(data)
#' winkel_sector <- 22.5
#' winkel_half <- winkel_sector / 2
#' wide <- dplyr::mutate(
#'   wide,
#'   wd_shifted = (.data$wd + winkel_half) %% 360,
#'   wd_class = ggplot2::cut_width(.data$wd_shifted, center = winkel_half, width = winkel_sector),
#'   ws_class = forcats::fct_rev(ggplot2::cut_interval(.data$ws, length = 2))
#' )
#'
#' # background map
#' bbox <- tibble::tibble(x = c(2683141 - 500, 2683141 + 500), y = c(1249040 - 500, 1249040 + 500))
#' bbox <- rOstluft::transform_LV95_to_WSG84(bbox)
#'
#' bbox <- c(left = bbox$lon[1], right = bbox$lon[2], bottom = bbox$lat[1], top = bbox$lat[2])
#'
#' raster_map <- ggmap::get_stamenmap(bbox, zoom = 16, maptype = "terrain",
#'                                    source = "stamen", color = "bw")
#'
#'
#' wind_rose <- ggplot(wide, aes(x = wd_class, fill = ws_class, y = stat(count / sum(count)))) +
#'   geom_bar(width=1, colour="grey80", size=0.5, alpha = 0.5, show.legend = TRUE) +
#'   scale_x_discrete(drop = FALSE, na.translate = FALSE) +
#'   scale_y_discrete(labels = function(x) {stringr::str_c(x*100, " %") }) +
#'   scale_fill_viridis_d(direction = 1)
#'
#' wind_rose + coord_polar2(start = - winkel_half * pi / 180, bg = raster_map)
coord_polar2 <- function(theta = "x", start = 0, direction = 1, clip = "on", bg = NULL) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"

  ggproto(NULL, CoordPolar2,
          theta = theta,
          r = r,
          start = start,
          direction = sign(direction),
          clip = clip,
          bg = bg
  )
}

tmp <- new.env()

#' @rdname rOstluft-ggproto
#' @export
CoordPolar2 <- ggproto("CoordPolar2", CoordPolar,

  render_bg = function(self, panel_params, theme) {
    panel_params <- rename_data(self, panel_params)

    tmp$polar2 <- self

    theta <- if (length(panel_params$theta.major) > 0)
      theta_rescale(self, panel_params$theta.major, panel_params)
    thetamin <- if (length(panel_params$theta.minor) > 0)
      theta_rescale(self, panel_params$theta.minor, panel_params)
    thetafine <- seq(0, 2 * pi, length.out = 100)

    rfine <- c(r_rescale(self, panel_params$r.major, panel_params$r.range), 0.45)

    # This gets the proper theme element for theta and r grid lines:
    #   panel.grid.major.x or .y
    majortheta <- paste("panel.grid.major.", self$theta, sep = "")
    minortheta <- paste("panel.grid.minor.", self$theta, sep = "")
    majorr     <- paste("panel.grid.major.", self$r,     sep = "")

    # render background map if necessary
    if (!is.null(self$bg)) {
      background <- grid::rasterGrob(self$bg, width = 1, height = 1, name = "bg")
    } else {
      background <- element_render(theme, "panel.background")
    }

    ggname("grill", grid::grobTree(
      background,
      if (length(theta) > 0) element_render(
        theme, majortheta, name = "angle",
        x = c(rbind(0, 0.45 * sin(theta))) + 0.5,
        y = c(rbind(0, 0.45 * cos(theta))) + 0.5,
        id.lengths = rep(2, length(theta)),
        default.units = "native"
      ),
      if (length(thetamin) > 0) element_render(
        theme, minortheta, name = "angle",
        x = c(rbind(0, 0.45 * sin(thetamin))) + 0.5,
        y = c(rbind(0, 0.45 * cos(thetamin))) + 0.5,
        id.lengths = rep(2, length(thetamin)),
        default.units = "native"
      ),

      element_render(
        theme, majorr, name = "radius",
        x = rep(rfine, each = length(thetafine)) * sin(thetafine) + 0.5,
        y = rep(rfine, each = length(thetafine)) * cos(thetafine) + 0.5,
        id.lengths = rep(length(thetafine), length(rfine)),
        default.units = "native"
      )
    ))
  }
)

rename_data <- function(coord, data) {
  if (coord$theta == "y") {
    rename(data, c("y" = "theta", "x" = "r"))
  } else {
    rename(data, c("y" = "r", "x" = "theta"))
  }
}

theta_rescale_no_clip <- function(coord, x, panel_params) {
  rotate <- function(x) (x + coord$start) * coord$direction
  rotate(rescale(x, c(0, 2 * pi), panel_params$theta.range))
}

theta_rescale <- function(coord, x, panel_params) {
  x <- scales::squish_infinite(x, panel_params$theta.range)
  rotate <- function(x) (x + coord$start) %% (2 * pi) * coord$direction
  rotate(scales::rescale(x, c(0, 2 * pi), panel_params$theta.range))
}

r_rescale <- function(coord, x, range) {
  x <- scales::squish_infinite(x, range)
  scales::rescale(x, c(0, 0.4), range)
}
