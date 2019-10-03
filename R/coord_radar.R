#' Polar coordinate system without munching
#'
#' @description based on [coord_polar2()], but paths are connected linear. See the documentation
#' of [coord_polar2()] for more Information and examples
#'
#'
#' @inheritParams coord_polar2
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_2010-2014.csv")
#' data <- rOstluft::read_airmo_csv(fn)
#' data <- rOstluft::rolf_to_openair(data)
#'
#' funs <- list(
#'   "median",
#'   "mean",
#'   "q95" = ~ stats::quantile(., probs = 0.95)
#' )
#'
#' data_summarized <- summary_wind(data, ws, wd, NOx, fun = funs,
#'   ws_cutfun = cut_number.fun(1)
#' )
#'
#' # background map
#' bbox <- bbox_lv95(2683141, 1249040, 500)
#' bg <- get_stamen_map(bbox)
#'
#' radar <- ggplot(data_summarized, aes(x = wd, y = NOx, color = stat, group = stat)) +
#'   geom_polygon(size = 1, fill = NA) +
#'   scale_color_viridis_d(end = 0.6) +
#'   scale_y_continuous(limits = c(0, NA), expand = c(0,0, 0, 0))
#'
#' radar + coord_radar(start = - 22.5 / 180 * pi)
#'
#' # add background map
#' radar + coord_radar(start = - 22.5 / 180 * pi, bg = bg)
#'
#' # facets
#' radar + facet_wrap(vars(stat)) +
#'   coord_radar(start = - 22.5 / 180 * pi)
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
