ggyearcourse <- function (data, y = "value", fun = list(middle = "mean", middle2 = "median",
                                        ymin = "percentile05", lower = "percentile25", upper = "percentile75",
                                        ymax = "percentile95"), nmin = 3, ribbon_color = "gray60",
          middle_color = "gray30", expand_xscale = 0.025) {
  groupings = grp("week")
  fun.args = list(na.rm = TRUE)
  data_summarized <- summary_periodic(data, y = y, groupings = groupings,
                                      fun = fun, fun.args = fun.args, nmin = nmin)
  data_summarized <- tidyr::spread(data_summarized, "stat",
                                   "value")
  data_summarized <- dplyr::mutate(data_summarized, parameter = factor(stringr::str_c(.data$parameter,
                                                                                      " (", .data$unit, ")")))
  times_of_day <- levels(data_summarized$starttime_of_day)
  if (length(times_of_day) < 24) {
    rlang::warn("creating diurnal diagramm with resolution greater then h1.")
  }
  breaks <- seq(length.out = 8, by = length(times_of_day)/8)
  labels <- times_of_day[breaks]
  plot <- ggplot(data_summarized, mapping = aes(x = as.numeric(.data$starttime_of_day),
                                                y = .data$middle, group = NA)) + geom_line(aes(y = .data$ymin),
                                                                                           lty = 2, color = ribbon_color) + geom_ribbon(aes(ymin = .data$lower,
                                                                                                                                            ymax = .data$upper), fill = ribbon_color, alpha = 0.5) +
    geom_line(aes(y = .data$middle2), lty = 2, color = middle_color) +
    geom_line(aes(y = .data$ymax), lty = 2, color = ribbon_color) +
    geom_line(color = middle_color) + lemon::facet_rep_grid(parameter ~
                                                              ., scales = "free_y", switch = "y") + scale_x_continuous(breaks = breaks,
                                                                                                                       labels = labels, expand = expansion(mult = expand_xscale)) +
    theme_rop_diuarnal() + theme(axis.title = element_blank())
  return(plot)
}
