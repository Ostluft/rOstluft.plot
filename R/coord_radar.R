#' Coordinate system for ggradar()
#'
#' based on coord_polar(), but paths are connected linear 
#' 
#' @export
coord_radar <- function (bg =NULL, theta = "x", start = 0, direction = 1) { # sneaked from here: http://www.cmap.polytechnique.fr/~lepennec/R/Radar/RadarAndParallelPlots.html
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}