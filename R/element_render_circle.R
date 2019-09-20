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
