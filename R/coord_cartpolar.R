#' cartesian coordinate system with polar grid lines
#'
#' @description
#' WIP:
#'
#' * render_axis_h / render_axis_v probably needs an overwrite
#' * labels for polar grid lines
#' * wd scale / breaks but how exactly?
#'
#'
#' @param limit limit for coordsystem (xlim(-limit, limit), ylim(-limit, limit))
#' @param expand expand limits (probably not used at the moment)
#' @param clip clip panel (not sure if used)
#' @param bg raster for background image
#' @param scale_wd pass breaks angle lines (not used at the moment)
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' df <- tibble::tribble(
#'   ~wd, ~ws, ~facet,
#'   0, 4, 1,
#'   90, 4, 1,
#'   180, 4, 1,
#'   270, 4, 1,
#'   45, 8, 2,
#'   135, 8, 2,
#'   225, 8, 2,
#'   315, 8, 2
#' )
#'
#' df <- dplyr::mutate(df,
#'   u = .data$ws * sin(2 * pi * .data$wd / 360),
#'   v = .data$ws * cos(2 * pi * .data$wd / 360)
#' )
#'
#' p <- ggplot(df, aes(x=u, y=v)) + geom_point() + coord_cartpolar()
#' p
#'
#'
#' # theming for panel.grid.major.x and panel.grid.minor.x is applied to polar grid lines
#' p + theme(
#'       "panel.grid.minor.x" = element_line(color = "blue"),
#'       "panel.grid.major.x" = element_line(color = "red", size = 2)
#'     )
#'
#' # x breaks are polar grid radiuses:
#' p + scale_x_continuous(breaks = c(1, 1.5, 3, 6)) +
#'   theme("panel.grid.minor.x" = element_blank())
#'
#'
#' # background map
#' bbox <- tibble::tibble(x = c(2683141 - 500, 2683141 + 500), y = c(1249040 - 500, 1249040 + 500))
#' bbox <- rOstluft::transform_projection(bbox, coord = c("x", "y"),
#'                                        initCRS = sp::CRS("+init=epsg:2056"),
#'                                        outCRS = sp::CRS("+init=epsg:4326"))
#'
#' bbox <- c(left = bbox$x[1], right = bbox$x[2], bottom = bbox$y[1], top = bbox$y[2])
#'
#' raster_map <- ggmap::get_stamenmap(bbox, zoom = 16, maptype = "terrain",
#'                                    source = "stamen", color = "bw")
#'
#' pbg <- ggplot(df, aes(x=u, y=v)) + geom_point() + coord_cartpolar(bg = raster_map)
#' pbg
#'
#'
#' # faceting
#' pbg + facet_wrap(~facet)
#'
#'
#' # if plotting a raster layer, use annotation_raster, panel.ontop and panel.background
#' df <- expand.grid(u = -5:5, v = -5:5)
#' df$z <- runif(nrow(df))
#' ggplot(df, aes(x=u, y=v, fill=z)) + coord_cartpolar(expand = FALSE) +
#'   annotation_raster(raster_map, -Inf, Inf, -Inf, Inf) +
#'   geom_raster(alpha = 0.5) +
#'   theme(
#'     "panel.ontop" = TRUE,
#'     "panel.background" = element_blank()
#'   )
#'
coord_cartpolar <- function(limit = NULL, expand = TRUE, clip = "on", bg = NULL, scale_wd = NULL) {
  ggproto(NULL, CoordCartPolar,
    limit = limit,
    expand = expand,
    clip = clip,
    bg = bg,
    scale_wd = scale_wd
  )
}


# assign values to inspect after executing ggproto functions
tmp <- new.env()


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
CoordCartPolar <- ggproto("CoordCartPolar", CoordCartesian,
  is_free = function() FALSE,

  aspect = function(self, ranges) 1,

  render_bg = function(self, panel_params, theme) {
    tmp$panel_params <- panel_params

    major_element <- calc_element("panel.grid.major.x", theme)
    minor_element <- calc_element("panel.grid.minor.x", theme)
    panel_element <- calc_element("panel.background", theme)

    elements = list()

    if (!is.null(self$bg)) {
      elements$bg <- grid::rasterGrob(self$bg, width = 1, height = 1, name = "bg")
    } else if (inherits(panel_element, "element_rect")) {
      elements$panel <- element_grob(panel_element)
    }

    if (inherits(minor_element, "element_line")) {
      tmp$minor <- minor_element
      xminor <- as.numeric(purrr::keep(panel_params$x.minor - 0.5, ~ . > 0))
      elements$xminor <- element_grob.element_circle(minor_element, fill = NA,
        x = rep(0.5, length(xminor)), y = rep(0.5, length(xminor)), r = xminor
      )
    }

    if (inherits(major_element, "element_line")) {
      tmp$major <- major_element
      xmajor <- as.numeric(purrr::keep(panel_params$x.major - 0.5, ~ . > 0))
      elements$xmajor <- element_grob.element_circle(major_element, fill = NA,
        x = rep(0.5, length(xmajor)), y = rep(0.5, length(xmajor)), r = xmajor
      )
    }

    rlang::exec(grid::grobTree, !!!elements, name = "grill")
  },

  setup_panel_params = function(self, scale_x, scale_y, params) {
    train_cartpolar <- function(scale, limits, name) {
      scale$train(limits)
      scale$limits <- limits
      res <- scale$break_info(limits)
      res$arrange <- scale$axis_order()
      rlang::set_names(res , paste(name, names(res), sep = "."))
    }

    lim <- max(abs(c(scale_x$range$range, scale_y$range$range)))

    if (!is.null(self$limit)) {
      limits <- c(-self$limit, self$limit)
    } else {
      # check for expand?
      if (isTRUE(self$expand)) {
        lim <- lim * 1.1
      }
      limits <- c(-lim, lim)
      self$limit <- lim  # set self$limit to prevent expanding in each facet
    }

    # probably not necessary
    self$limits$x <- limits
    self$limits$y <- limits

    c(
      train_cartpolar(scale_x, limits, "x"),
      train_cartpolar(scale_y, limits, "y")
    )
  }
)




# Name ggplot grid object
# Convenience function to name grid objects
# extracted from ggplot2/R/utilities-grid.R
#
# @keyword internal
ggname <- function(prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}

len0_null <- function(x) {
  if (length(x) == 0)
    NULL
  else
    x
}

# More performant modifyList without recursion
modify_list <- function(old, new) {
  for (i in names(new)) old[[i]] <- new[[i]]
  old
}

element_grob.element_circle <- function(element, x = 0.5, y = 0.5, r = 1,
  fill = NULL, colour = NULL, size = NULL, linetype = NULL, ...) {

  # The gp settings can override element_gp
  gp <- grid::gpar(lwd = len0_null(size * .pt), col = colour, fill = fill, lty = linetype)
  element_gp <- grid::gpar(lwd = len0_null(element$size * .pt), col = element$colour,
    fill = element$fill)

  gp = modify_list(element_gp, gp)

  grid::circleGrob(x, y, r, gp = gp, ...)
}
