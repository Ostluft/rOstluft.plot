#' ggplot2 theme for ggtraj()
#' @keywords internal
#' @noRd
theme_traj <-
  ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  )


#' ggplot2 theme for ggpolarplot()
#' @keywords internal
#' @noRd
theme_polarplot <-
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.ticks.y = ggplot2::element_line(),
    panel.ontop = TRUE,
    panel.grid.minor.y = ggplot2::element_line(linetype = 2, color = "gray80", size = 0.5),
    panel.grid.minor.x = ggplot2::element_line(linetype = 2, color = "gray80", size = 0.5),
    panel.grid.major = ggplot2::element_line(linetype = 2, color = "gray80", size = 0.5),
    axis.text.x = element_blank(),
    axis.title = element_blank()
  )


#' ggplot2 theme for ggradar()
#' @keywords internal
#' @noRd
theme_radar <-
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.ticks.y = ggplot2::element_line(),
    panel.ontop = FALSE,
    panel.grid.minor = element_blank(),
    panel.grid.major = ggplot2::element_line(linetype = 2, color = "gray80", size = 0.5),
    axis.title.x = element_blank()
  )


#' ggplot2 theme for ggwindrose
#' @keywords internal
#' @noRd
theme_windrose <-
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.ticks.y = ggplot2::element_line(),
    panel.ontop = FALSE,
    panel.grid.minor.x = element_blank(),
    panel.grid.major = ggplot2::element_line(linetype = 2, color = "gray80", size = 0.5),
    axis.title = element_blank()
  )

