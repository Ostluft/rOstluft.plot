#' themes for rOstluft plots
#'
#' @description this themes are used in the respective gg wrapper to apply some
#' basic theming.
#'
#' @param base a ggplot2 theme. see [ggplot2::ggtheme]
#'
#' @examples
#' library(ggplot2)
#' fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_2010-2014.csv")
#' data <- rOstluft::read_airmo_csv(fn)
#' data <- rOstluft::rolf_to_openair(data)
#' data <- dplyr::mutate(data, year = lubridate::year(date))
#'
#' data_summarized <- summary_wind(data, ws, wd, ws,
#'   ws_cutfun = cut_ws.fun(ws_max = 4, reverse = TRUE)
#' )
#'
#' p <- ggplot(data_summarized, aes(x = wd, y = freq, fill = ws)) +
#'   geom_bar(stat = "identity") +
#'   coord_polar2(start = - 22.5 / 180 * pi ) +
#'   scale_y_continuous(
#'     limits = c(0, NA),
#'     expand = c(0,0, 0, 0),
#'     labels = scales::percent
#'   ) +
#'   scale_fill_viridis_d()
#'
#' # default appearance
#' p
#'
#' # with rOstluft theming for a windrose
#' p + theme_rop_windrose()
#'
#' # prefer bw as base and a bigger font for a presentation
#' p + theme_rop_windrose(
#'   theme_bw(base_size = 14)
#' )
#'
#' @name themes_rop
#' @aliases NULL
NULL

#' @rdname themes_rop
#' @export
theme_rop_traj <- function(base = ggplot2::theme_minimal()) {
  base +ö
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  )
}

#' @rdname themes_rop
#' @export
theme_rop_polarplot <- function(base = ggplot2::theme_minimal()) {
  base +
  ggplot2::theme(
    axis.ticks.y = ggplot2::element_line(),
    panel.grid.minor.y = ggplot2::element_line(linetype = 2, color = "gray80", size = 0.5),
    panel.grid.minor.x = ggplot2::element_line(linetype = 2, color = "gray80", size = 0.5),
    panel.grid.major = ggplot2::element_line(linetype = 2, color = "gray80", size = 0.5),
    axis.text.x = element_blank(),
    axis.title = element_blank()
  )
}

#' @rdname themes_rop
#' @export
theme_rop_radar <- function(base = ggplot2::theme_minimal()) {
  base +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.ticks.y = ggplot2::element_line(),
    panel.ontop = FALSE,
    panel.grid.minor = element_blank(),
    panel.grid.major = ggplot2::element_line(linetype = 2, color = "gray80", size = 0.5),
    axis.title.x = element_blank()
  )
}

#' @rdname themes_rop
#' @export
theme_rop_windrose <- function(base = ggplot2::theme_minimal()) {
  base +
  ggplot2::theme(
    axis.ticks.y = ggplot2::element_line(),
    panel.ontop = FALSE,
    panel.grid.minor.x = element_blank(),
    panel.grid.major = ggplot2::element_line(linetype = 2, color = "gray80", size = 0.5),
    axis.title = element_blank()
  )
}
