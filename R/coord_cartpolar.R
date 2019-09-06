#' cartesian coordinate system with polar grid lines
#'
#' @description A cartesian coordinate system overlayed with a polar grid.
#' It is used together with [summary_wind_2d()] in [ggpolarplot()].
#'
#' @section TODO:
#' - optimize scaling to match [coord_polar2()] / [ggplot2::polar()]
#' - Option to plot only positive y breaks
#'
#' @param limit limit for coordsystem (xlim(-limit, limit), ylim(-limit, limit))
#' @param expand expand limits
#' @param clip clip panel (not sure if used)
#' @param bg raster for background image
#' @param scale_wd pass breaks angle lines
#' @param labels_wd [ggplot2::element_text()] object to format wd labels. Use [ggplot2::element_blank()]
#'   to draw nothing
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
#'   30, 8, 2,
#'   120, 8, 2,
#'   200, 8, 2,
#'   300, 8, 2
#' )
#'
#' df <- dplyr::mutate(df,
#'   u = .data$ws * sin(2 * pi * .data$wd / 360),
#'   v = .data$ws * cos(2 * pi * .data$wd / 360)
#' )
#'
#' p <-  ggplot(df, aes(x=u, y=v)) + geom_point() + coord_cartpolar()
#' p
#'
#' # theming for panel.grid.major.x and panel.grid.minor.x is applied to polar grid lines
#' # panel.grid.major.y is applied to the wd lines, to format the labels use an element_text
#' # in the coord_cartpolar constructor as arugment labels_wd
#' ggplot(df, aes(x=u, y=v)) + geom_point() +
#'   coord_cartpolar(labels_wd = element_text(color = "violet", size = 14, face = "bold")) +
#'   theme(
#'       "panel.grid.minor.x" = element_line(color = "blue", linetyp = "dotted"),
#'       "panel.grid.major.x" = element_line(color = "red", size = 2, linetyp = "dashed"),
#'       "panel.grid.major.y" = element_line(color = "green", arrow = arrow()),
#'     )
#'
#' # x breaks are polar grid radiuses:
#' p + scale_x_continuous(breaks = c(1, 2.5, 5, 10)) +
#'   theme("panel.grid.minor.x" = element_blank())
#'
#' # wd scale in coord_cartpolar constructor as arugment scale_wd
#' ggplot(df, aes(x=u, y=v)) + geom_point() +
#'   coord_cartpolar(scale_wd = scale_wd_identity(
#'     breaks = seq(0, 315, 45), labels = c("N", "NO", "O", "SO", "S", "SW", "W", "NW")
#'   ))
#'
#'
#' # background map
#' bb <- bbox_lv95(2683141, 1249040, 500)
#' bg <- get_stamen_map(bb)
#' pbg <- ggplot(df, aes(x=u, y=v)) + geom_point() + coord_cartpolar(bg = bg)
#' pbg
#'
#' # faceting
#' pbg + facet_wrap(vars(facet))
#'
#' # if plotting a raster layer, use annotation_raster, panel.ontop and panel.background
#' df <- expand.grid(u = -5:5, v = -5:5)
#' df$z <- runif(nrow(df))
#' ggplot(df, aes(x=u, y=v, fill=z)) + coord_cartpolar(expand = FALSE) +
#'   annotation_raster(bg, -Inf, Inf, -Inf, Inf) +
#'   geom_raster(alpha = 0.5) +
#'   theme(
#'     "panel.ontop" = TRUE,
#'     "panel.background" = element_blank()
#'   )
coord_cartpolar <- function(limit = NULL, expand = TRUE, clip = "on", bg = NULL, scale_wd = scale_wd_identity(),
                            labels_wd = element_text()) {
  ggproto(NULL, CoordCartPolar,
    limit = limit,
    expand = expand,
    clip = clip,
    bg = bg,
    scale_wd = scale_wd,
    labels_wd = labels_wd
  )
}

#' @rdname rOstluft-ggproto
#' @export
CoordCartPolar <- ggproto("CoordCartPolar", CoordCartesian,
  is_free = function() FALSE,

  aspect = function(self, ranges) 1,

  render_bg = function(self, panel_params, theme) {
    elements = list()

    xmajor_element <- calc_element("panel.grid.major.x", theme)
    xminor_element <- calc_element("panel.grid.minor.x", theme)
    xminor <- xmajor <- -1

    if (!is.null(self$bg)) {
      elements$bg <- grid::rasterGrob(self$bg, width = 1, height = 1, name = "bg")
    } else {
      elements$panel <- element_render(theme, "panel.background")
    }

    if (inherits(xminor_element, "element_line")) {
      xminor <- setdiff(panel_params$x.minor, panel_params$x.major)
      xminor <- as.numeric(purrr::keep(xminor - 0.5, ~ . > 0))
      elements$xminor <- element_render_circle(theme, "panel.grid.minor.x", name = "rxminor",
        x = rep(0.5, length(xminor)), y = rep(0.5, length(xminor)), r = xminor
      )
    }

    if (inherits(xmajor_element, "element_line")) {
      xmajor <- as.numeric(purrr::keep(panel_params$x.major - 0.5, ~ . > 0))
      elements$xmajor <- element_render_circle(theme, "panel.grid.major.x", name = "rxmajor",
        x = rep(0.5, length(xmajor)), y = rep(0.5, length(xmajor)), r = xmajor
      )
    }

    if(!is.null(self$scale_wd)) {

      line_length <- max(xminor, xmajor)
      if (line_length < 0) line_length <- 0.4

      theta <- self$scale_wd$get_breaks() / 180 * pi

      if (length(theta) > 0) {
        x0 <- line_length * sin(theta) + 0.5
        y0 <- line_length * cos(theta) + 0.5

        x <- c(rbind(rep(0.5, length(x0)), x0))
        y <- c(rbind(rep(0.5, length(y0)), y0))
        id.lengths <- rep(2, length(x0))

        elements$ymajor <- element_render(theme, "panel.grid.major.y", x = x, y = y, id.lengths = id.lengths, name = "wdmajor")
      }

      labels <- self$scale_wd$get_labels()

      if (length(labels) > 0 & length(theta) > 0) {
        line_length <- min(line_length + (0.5 - line_length) / 2, 0.45)
        x0 <- line_length * sin(theta) + 0.5
        y0 <- line_length * cos(theta) + 0.5

        elements$labels <- ggname("wdlabels", element_grob(self$labels_wd, labels, x0, y0, hjust = 0.5, vjust = 0.5))
      }
    }

    rlang::exec(grid::grobTree, !!!elements, name = "grill")
  },

  setup_panel_params = function(self, scale_x, scale_y, params) {
    train_cartpolar <- function(scale, limits, name, order) {
      scale$train(limits)
      scale$limits <- limits
      res <- scale$break_info(limits)
      res$arrange <- order
      rlang::set_names(res , paste(name, names(res), sep = "."))
    }

    lim <- max(abs(c(scale_x$range$range, scale_y$range$range))) * sqrt(2)

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
      train_cartpolar(scale_x, limits, "x", scale_x$axis_order()),
      train_cartpolar(scale_x, limits, "y", scale_y$axis_order())
    )
  }
)

#' predifend scale for NOSW Labels for coord_cartpolar
#'
#' @param breaks on wd scale
#' @param labels for wd scale
#' @param ... passed onto [ggplot2::scale_continuous_identity()]
#'
#' @return [ggplot2::scale_continuous_identity()]
#' @export
#'
#' @rdname coord_cartpolar
scale_wd_identity <- function(breaks = c(0, 90, 180, 270), labels = c("N", "O", "S", "W"), ...) {
  scale_continuous_identity(aesthetics = "wd",  breaks = breaks, labels = labels, limits = c(0, 360), ...)
}


#' renders a circle as element_line
#'
#' @param theme A `theme` object
#' @param element The name of a theme element (see [ggplot2::calc_element()]).
#' @param x a numeric vector specifying x center locations of circles
#' @param y a numeric vector specifying y center locations of circles
#' @param r a numeric vector specifying radii of circles
#' @param ... passed onto [ggplot2::element_grob()]
#' @param name of generated grid object
#' @param nseg number of line segments to draw the circle
#'
#' @return [ggplot2::zeroGrob()] or [grid::polylineGrob()]
#' @export
element_render_circle <- function(theme, element, x, y, r, ..., name = NULL, nseg = 360) {
  thetafine <- seq(0, 2 * pi, length.out = nseg)  # angle as sequence in radiants

  element_render(
    theme, element, name = name,
    x = rep(r, each = nseg) * sin(thetafine) + rep(x, each = nseg),
    y = rep(r, each = nseg) * cos(thetafine) + rep(y, each = nseg),
    id.lengths = rep(nseg, length(r)),
    default.units = "native"
  )
}

