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
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(linetype = 2, color = "gray80")
  )



theme_radar <- theme_polarplot + theme(panel.ontop = FALSE)


