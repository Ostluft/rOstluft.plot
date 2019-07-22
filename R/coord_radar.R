#' Coordinate system for ggradar()
#'
#' @description based on coord_polar(), but paths are connected linear
#' @inheritParams coord_polar2
#'
#' @export
coord_radar <- function (theta = "x", start = 0, direction = 1, bg = NULL) {
  # sneaked from here: http://www.cmap.polytechnique.fr/~lepennec/R/Radar/RadarAndParallelPlots.html
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar2,
    theta = theta,
    r = r,
    start = start,
    direction = sign(direction),
    bg = bg,
    is_linear = function(coord) TRUE
  )
}
