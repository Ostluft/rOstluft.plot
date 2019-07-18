theme_traj <-
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  )



theme_polarplot <-
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.ticks.y = ggplot2::element_line(),
    panel.ontop = TRUE,
    panel.grid.minor.y = ggplot2::element_line(linetype = 2, color = "gray60", size = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.major = ggplot2::element_line(linetype = 2, color = "gray60", size = 0.5),
    axis.text.x = element_blank(),
    axis.title.y = element_blank()
  )



theme_radar <-
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.ticks.y = ggplot2::element_line(),
    panel.ontop = FALSE,
    panel.grid.minor = element_blank(),
    panel.grid.major = ggplot2::element_line(linetype = 2, color = "gray60", size = 0.5),
    axis.title.x = element_blank()
  )



theme_windrose <-
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.ticks.y = ggplot2::element_line(),
    panel.ontop = FALSE,
    panel.grid.minor.x = element_blank(),
    panel.grid.major = ggplot2::element_line(linetype = 2, color = "gray60", size = 0.5),
    axis.title = element_blank()
  )

