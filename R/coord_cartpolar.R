#' cartesian coordinate system with polar grid lines
#'
#' @description A cartesian coordinate system overlayed with a polar grid.
#' It is used together with [summary_wind_2d()] in [ggpolarplot()].
#'
#' @section Warning:
#' This coordinate system bends some ggplot2 internals (eg. diverted x axis). It works
#' for our scope, but the testing was surficial. And probably some things could easily
#' break.
#'
#' @param limit limit for coordsystem (xlim(-limit, limit), ylim(-limit, limit))
#' @param expand if `TRUE`
#' @param clip clip panel (not sure if used)
#' @param bg raster for background image
#' @param grid draw the lines and labels of the polar grid in the foreground or
#'   background
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' df <- tibble::tribble(
#'   ~wd, ~ws, ~facet,
#'   0, 5, 1,
#'   0, 2.5, 1,
#'   0, 9.5, 1,
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
#' p <-  ggplot(df, aes(x=u, y=v)) + geom_point()
#'
#' p + coord_cartpolar()
#'
#'
#' # y scale is used for uv scaling (negative breaks are ignored)
#' p  + coord_cartpolar() +
#'   scale_y_continuous(
#'     limits = c(-10, 10),
#'     breaks = c(-5, 0, 1, 2.5, 7.5, 10),
#'     expand = c(0, 0, 0, 0)
#'   )
#'
#' # alternative argument limit of coord_cartpolar can be used
#' p  + coord_cartpolar(limit = 5)
#'
#' # a continuous x scale is used to control the breaks and labels
#' # for wd. Setting limits in this scale will cause problems
#' p  + coord_cartpolar() +
#'   scale_x_continuous(
#'     breaks = c(0, 45, 90, 180, 270),
#'     labels = c("Nord", "NE", "E", "S", "W")
#'   )
#'
#'
#' # theming: only major grid is draw
#' p + coord_cartpolar() +
#'   theme(
#'     "panel.grid.major.x" = element_line(color = "red", size = 2, linetyp = "dashed"),
#'     "panel.grid.major.y" = element_line(color = "green", arrow = arrow()),
#'     "axis.text.x" = element_text(color = "violet", size = 14, face = "bold"),
#'     "axis.text.y" = element_text(color = "blue")
#'   )
#'
#'
#' # background map
#' bb <- bbox_lv95(2683141, 1249040, 500)
#' bg <- get_stamen_map(bb)
#'
#' p + coord_cartpolar(bg = bg)
#'
#' # faceting
#' p + coord_cartpolar(bg = bg) +
#'   facet_wrap(vars(facet))
#'
#' # if plotting a raster layer, use grid = "foreground" to draw
#' # the polar grid over the raster and set the limit to max + 0.5
#' # and all expands to zero
#' raster <- expand.grid(u = -10:10, v = -10:10)
#' raster$z <- runif(nrow(raster))
#' ggplot(raster, aes(x=u, y=v, fill=z)) +
#'   coord_cartpolar(bg = bg, grid = "foreground",  limit = 10.5) +
#'   geom_raster(alpha = 0.5) +
#'   scale_y_continuous(expand = c(0, 0, 0, 0))
coord_cartpolar <- function(limit = NA, expand = TRUE, clip = "on",
                             bg = NULL, grid = c("background", "foreground")) {
  grid <- match.arg(grid)

  ggproto(NULL, CoordCartPolar,
    limit = limit,
    expand = expand,
    clip = clip,
    bg = bg,
    grid = grid
  )
}

#' @rdname rOstluft-ggproto
#' @export
CoordCartPolar <- ggproto("CoordCartPolar", CoordCartesian,
  is_free = function() FALSE,

  aspect = function(self, ranges) 1,

  backtransform_range = function(self, panel_params) {
    self$range(panel_params)
  },

  range = function(self, panel_params) {
    # summarise_layout() expects that the x and y ranges here
    # match the setting from self$theta and self$r
    setNames(
      list(panel_params$uv.range, panel_params$uv.range),
      c("x", "y")
    )
  },

  transform = function(self, data, panel_params) {
    trans_xy <- function(data) uv_rescale(data, panel_params$uv.range)
    ggplot2::transform_position(data, trans_xy, trans_xy)
  },

  setup_panel_params = function(self, scale_x, scale_y, params) {

    if (is.na(self$limit)) {
      if (is.null(scale_y$limits) || all(is.na(scale_y$limits))) {
        uv_range <- c(scale_x$range$range, scale_y$range$range)
        self$limit <- max(abs(uv_range))
      } else {
        self$limit <- max(abs(scale_y$limits), na.rm = TRUE)
      }
    }

    scale_y$limits <- c(-self$limit, self$limit)

    if (isTRUE(self$expand)) {
      expand <- expand_default(scale_y)
    } else {
      expand <- c(0, 0)
    }

    uv_range <- scale_y$dimension(expand)
    uv_info <- scale_y$break_info(uv_range)
    uv_info$major <- uv_rescale(uv_info$major, c(0,1))
    uv_info$minor <- uv_rescale(uv_info$minor, c(0,1))
    uv_info$arrange <- scale_y$axis_order()
    uv_info <- rlang::set_names(uv_info, paste("uv.", names(uv_info), sep = ""))

    if (scale_x$is_discrete()) {
      stop("coord_cartpolar doesn't support discrete x scale")
    }

    if (is.waive(scale_x$breaks)) {
      scale_x$breaks <- c(0, 90, 180, 270)
    }

    if (is.waive(scale_x$labels)) {
      scale_x$labels <- c("N", "O", "S", "W")
    }

    wd_info <- scale_x$break_info(c(0, 360))
    wd_info$major <- wd_rescale(wd_info$major)
    wd_info$minor <- wd_rescale(wd_info$minor)
    wd_info$arrange <- scale_x$axis_order()
    wd_info <- rlang::set_names(wd_info, paste("wd.", names(wd_info), sep = ""))

    c(
      uv_info,
      wd_info
    )
  },


  render_bg = function(self, panel_params, theme) {
    elements = list()

    if (!is.null(self$bg)) {
      elements$bg <- grid::rasterGrob(self$bg, width = 1, height = 1, name = "bg")
    } else {
      elements$panel <- element_render(theme, "panel.background")
    }

    if (self$grid == "background") {
      elements <- c(elements, render_polargrid(self, panel_params, theme))
    }

    rlang::exec(grid::grobTree, !!!elements, name = "panel_background")
  },

  render_fg = function(self, panel_params, theme) {
    elements = list(
      "panel_border" = element_render(theme, "panel.border")
    )

    if (self$grid == "foreground") {
      elements <- c(elements, render_polargrid(self, panel_params, theme))
    }

    rlang::exec(grid::grobTree, !!!elements, name = "panel_foreground")
  },

  # use the render_axis_h method of CoordPolar
  # see https://stackoverflow.com/questions/55282193/call-setup-data-from-parent-class
  render_axis_h = .subset2(ggplot2::CoordPolar, "render_axis_h"),

  render_axis_v = function(self, panel_params, theme) {
    # only render the positiv side of the axis to emulate coord_polar
    major <- purrr::map_lgl(panel_params$uv.major_source, ~ . >= 0)
    minor <- purrr::map_lgl(panel_params$uv.minor_source, ~ . >= 0)


    # ggplot2::render_axis isn't exported but we can
    # rename the uv params to y and call the parent method
    panel_params <- list(
      y.range = panel_params$uv.range,
      y.labels = panel_params$uv.labels[major],
      y.major = panel_params$uv.major[major],
      y.major_source = panel_params$uv.major_source[major],
      y.minor = panel_params$uv.minor[minor],
      y.minor_source = panel_params$uv.minor_source[minor],
      y.arrange = panel_params$uv.arrange
    )

    ggproto_parent(CoordCartesian, self)$render_axis_v(panel_params, theme)
  }
)


uv_rescale <- function(uv, range) {
  uv <- scales::rescale(uv, c(0.10, 0.90), range)
  scales::squish_infinite(uv, c(0, 1))
}

wd_rescale <- function(wd) {
  scales::rescale(wd, c(0, 2 * pi), c(0, 1))
}

render_polargrid <- function(self, panel_params, theme) {
  elements = list()

  uvmajor_element <- calc_element("panel.grid.major.x", theme)


  if (inherits(uvmajor_element, "element_line")) {
    uvmajor <- as.numeric(purrr::keep(panel_params$uv.major - 0.5, ~ . > 0))
    # ymajor <- r_rescale(self, ymajor, panel_params$x.range)
    elements$uvmajor <- element_render_circle(theme, "panel.grid.major.x", name = "uvmajor",
      x = rep(0.5, length(uvmajor)), y = rep(0.5, length(uvmajor)), r = uvmajor
    )
  }

  line_length <- min(max(uvmajor, na.rm = TRUE), 0.4)
  wd <- panel_params$wd.major
  wd_labels <- panel_params$wd.labels

  if (length(wd) > 0) {
    x0 <- line_length * sin(wd) + 0.5
    y0 <- line_length * cos(wd) + 0.5

    x <- c(rbind(rep(0.5, length(x0)), x0))
    y <- c(rbind(rep(0.5, length(y0)), y0))
    id.lengths <- rep(2, length(x0))

    elements$wdmajor <- element_render(theme, "panel.grid.major.y", x = x, y = y,
                                       id.lengths = id.lengths, name = "wdmajor")
  }


  if (length(wd_labels) > 0 & length(wd) > 0) {
    x0 <- 0.45 * sin(wd) + 0.5
    y0 <- 0.45 * cos(wd) + 0.5
    elements$labels <- element_render(theme, "axis.text.x", wd_labels, x0, y0,
                                      hjust = 0.5, vjust = 0.5, name = "wdlabels")

  }

  elements
}


