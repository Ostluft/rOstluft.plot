#' Customized coord_polar
#'
#' The differences between [ggplot2::coord_polar()] and [coord_polar2()] are:
#' * Add a raster (image) as background. As example a map created with [get_stamen_map()]
#' * [ggplot2::coord_polar()] always adds an outer circle with r = 0.45 to the plot. This circle is removed.
#'
#' @param bg raster for background image
#' @inheritParams ggplot2::coord_polar
#'
#' @seealso `ggplot2::coord_polar()`
#'
#' @return ggplot2 coord system
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_2010-2014.csv")
#' data <- rOstluft::read_airmo_csv(fn) %>%
#'   rOstluft::rolf_to_openair()
#'
#' data_summarized <- summary_wind(data, ws, wd, ws,
#'   ws_cutfun = cut_ws.fun(ws_max = 4, reverse = TRUE)
#' )
#'
#' wind_rose <- ggplot(data_summarized, aes(x = wd, y = freq, fill = ws)) +
#'   geom_bar(stat = "identity", alpha = 0.8) +
#'   scale_y_continuous(
#'     limits = c(0, NA),
#'     expand = c(0,0, 0, 0),
#'     labels = scales::percent
#'   ) +
#'   scale_fill_viridis_d()
#'
#' # background map
#' bbox <- bbox_lv95(2683141, 1249040, 500)
#' bg <- get_stamen_map(bbox)
#'
#' wind_rose + coord_polar2(start = - 45 * pi / 360, bg = bg)
#'
#' # removed outer circle
#' df <- tibble::tibble(x = factor(1:10), y = runif(10, 0, 100))
#'
#' p <- ggplot(df, aes(x = x, y = y)) +
#'   geom_bar( stat = "identity") +
#'   scale_x_discrete() +
#'   theme(
#'     axis.line.x = element_line(colour = "red"),
#'     axis.line.y = element_line(colour = "orange"),
#'     panel.grid.major.x = element_line(colour = "blue"),
#'     panel.grid.major.y = element_line(colour = "darkgreen"),
#'     panel.grid.minor.y = element_line(colour = "darkolivegreen2")
#'   )
#'
#' # default behaviour with no breaks
#' p + coord_polar() + scale_y_continuous()
#'
#' p + coord_polar2() + scale_y_continuous()
#'
#' # behavoiur with manuel breaks and limit
#' p + coord_polar() + scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 200))
#'
#' p + coord_polar2() + scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 200))
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

#' @rdname rOstluft-ggproto
#' @export
CoordPolar2 <- ggproto("CoordPolar2", CoordPolar,

  render_bg = function(self, panel_params, theme) {
    panel_params <- rename_data(self, panel_params)

    theta <- if (length(panel_params$theta.major) > 0)
      theta_rescale(self, panel_params$theta.major, panel_params)
    thetamin <- if (length(panel_params$theta.minor) > 0)
      theta_rescale(self, panel_params$theta.minor, panel_params)
    thetafine <- seq(0, 2 * pi, length.out = 100)

    # , 0.45 adds the outer circle !
    # rfine <- c(r_rescale(self, panel_params$r.major, panel_params$r.range), 0.45)
    rfine <- r_rescale(self, panel_params$r.major, panel_params$r.range)
    if (length(rfine) > 0) {
      rmax <- max(rfine)
    } else {
      rfine <- rmax <- 0.40
    }

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
        x = c(rbind(0, rmax * sin(theta))) + 0.5,
        y = c(rbind(0, rmax * cos(theta))) + 0.5,
        id.lengths = rep(2, length(theta)),
        default.units = "native"
      ),
      if (length(thetamin) > 0) element_render(
        theme, minortheta, name = "angle",
        x = c(rbind(0, rmax * sin(thetamin))) + 0.5,
        y = c(rbind(0, rmax * cos(thetamin))) + 0.5,
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

theta_rescale <- function(coord, x, panel_params) {
  x <- scales::squish_infinite(x, panel_params$theta.range)
  rotate <- function(x) (x + coord$start) %% (2 * pi) * coord$direction
  rotate(scales::rescale(x, c(0, 2 * pi), panel_params$theta.range))
}

r_rescale <- function(coord, x, range) {
  x <- scales::squish_infinite(x, range)
  scales::rescale(x, c(0, 0.40), range)
}
