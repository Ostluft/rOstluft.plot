#' ggplot2-wrapper to summarise and plot data (of [rOstluft::format_rolf()] format) for diurnal time course-stats
#'
#' @param data a data.frame or tibble containing the data
#' @param y a character string specifying the taget variable to be summarised, usually "value".
#' outcomes of [rOstluft::cut_timeseries_periodic()].
#' @param fun function or list of functions for summary, can be named (then the outut stat is named after the function's name);
#'   Strings matching the regular expression `/^percentile([0-9]){1,2}$/i` are converted into the respective function.
#'   "percentile95" => `function(x, ...) quantile(x, 95 / 100, ...)`
#' @param ribbon_color character string for specifying the ribbon color.
#' @param middle_color character string for specifying the middle's color.
#' @param expand_xscale multiplicative range expansion factor
#'
#' @return [ggplot2::ggplot()] object
#' @export
#'
#' @examples
#' fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_2010-2014.csv")
#'
#' data_min30 <-
#'   rOstluft::read_airmo_csv(fn) %>%
#'   dplyr::filter(parameter %in% c("NOx", "O3", "PM10"))
#'
#' data_h1 <- rOstluft::resample(data_min30, new_interval = "h1")
#'
#' ggdiurnal(data_h1)
#'
#' # change scale_expands: less margin on x, no expand lower y and set to zero
#' ggdiurnal(data_h1, expand_xscale = 0.01) +
#'   scale_y_continuous(limits = c(0, NA), expand = expand_scale(mult = c(0, 0.05))) +
#'   theme(panel.spacing.y = unit(12, "pt"))
#'
#' # 30min resolution, add title, x/y label (must set theme elements)
#' ggdiurnal(data_min30) +
#'   ggtitle("Diurnal Zuerich Stampfenbachstrasse 2010-2014 ") +
#'   ylab("parameter") +
#'   xlab("time") +
#'   theme(
#'     axis.title = element_text(),
#'     axis.title.x = element_text()
#'   )
ggdiurnal <- function(
  data,
  y = "value",
  fun = list(middle = "mean", middle2 = "median", ymin = "percentile05",
             lower = "percentile25", upper = "percentile75", ymax = "percentile95"),
  nmin = 3,
  ribbon_color = "gray60",
  middle_color = "gray30",
  expand_xscale = 0.025
) {

  groupings = grp("starttime_of_day")
  fun.args = list(na.rm = TRUE)

  data_summarized <- summary_periodic(data, y = y, groupings = groupings, fun = fun, fun.args = fun.args, nmin = nmin)
  data_summarized <- tidyr::spread(data_summarized, "stat", "value")
  data_summarized <- dplyr::mutate(data_summarized,
    parameter = factor(stringr::str_c(.data$parameter, " (", .data$unit, ")"))
  )

  times_of_day <- levels(data_summarized$starttime_of_day)
  if (length(times_of_day) < 24) {
    rlang::warn("creating diurnal diagramm with resolution greater then h1.")
  }

  breaks <- seq(length.out = 8, by = length(times_of_day) / 8 )
  labels <- times_of_day[breaks]

  plot <-
    ggplot(data_summarized, mapping = aes(x = as.numeric(.data$starttime_of_day), y = .data$middle, group = NA)) +
    geom_line(aes(y = .data$ymin), lty = 2, color = ribbon_color) +
    geom_ribbon(aes(ymin = .data$lower, ymax = .data$upper), fill = ribbon_color, alpha = 0.5) +
    geom_line(aes(y = .data$middle2), lty = 2, color = middle_color) +
    geom_line(aes(y = .data$ymax), lty = 2, color = ribbon_color) +
    geom_line(color = middle_color) +
    lemon::facet_rep_grid(parameter~., scales = "free_y", switch = "y") + # ncol = ceiling(length(unique(data_summarized$parameter)) / 4)) +
    scale_x_continuous(
      breaks = breaks,
      labels = labels,
      expand = expand_scale(mult = expand_xscale)
    ) +
    theme_rop_diuarnal() +
    theme(
      axis.title = element_blank()
    )

  return(plot)
}



#' ggplot2-wrapper to summarise and plot data (of [rOstluft::format_rolf()] format) for diurnal time course-stats
#' including weekend-effect
#'
#' @param data a data.frame or tibble containing the data
#' @param y a character string specifying the taget variable to be summarised, usually "value".
#' outcomes of [rOstluft::cut_timeseries_periodic()].
#' @param fun function or list of functions for summary, can be named (then the outut stat is named after the function's name);
#'   Strings matching the regular expression `/^percentile([0-9]){1,2}$/i` are converted into the respective function.
#'   "percentile95" => `function(x, ...) quantile(x, 95 / 100, ...)`
#' @param ribbon_color character string for specifying the ribbon color.
#' @param middle_color character string for specifying the middle's color.
#' @param expand_xscale multiplicative range expansion factor
#'
#' @return [ggplot2::ggplot()] object
#' @export
#'
#' @examples
#' fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_2010-2014.csv")
#'
#' data <-
#'   rOstluft::read_airmo_csv(fn) %>%
#'   dplyr::filter(parameter %in% c("NOx", "O3", "PM10")) %>%
#'   rOstluft::resample(new_interval = "h1")
#'
#' ggdiurnal_weekend(data)
#'
#' # y scale: no expand on zero
#' ggdiurnal_weekend(data) +
#'   scale_y_continuous(limits = c(0, NA), expand = expand_scale(mult = c(0, 0.05))) +
#'   theme(panel.spacing.y = unit(12, "pt"))
ggdiurnal_weekend <- function(
  data,
  y = "value",
  nmin = 3,
  fun = list(middle = "mean", middle2 = "median", ymin = "percentile05",
             lower = "percentile25", upper = "percentile75", ymax = "percentile95"),
  ribbon_color = "gray60",
  middle_color = "gray30",
  expand_xscale = 0
) {

  groupings = grp("starttime_of_day", "weekend")
  fun.args = list(na.rm = TRUE)

  data_summarized <- summary_periodic(data, y = y, groupings = groupings, fun = fun, fun.args = fun.args, nmin = nmin)
  data_summarized <- tidyr::spread(data_summarized, "stat", "value")
  data_summarized <- dplyr::mutate(data_summarized,
    parameter = factor(stringr::str_c(.data$parameter, " (", .data$unit, ")"))
  )

  times_of_day <- levels(data_summarized$starttime_of_day)
  if (length(times_of_day) < 24) {
    rlang::warn("creating diurnal diagramm with resolution greater then h1.")
  }

  breaks <- seq(length.out = 4, by = length(times_of_day) / 4 )
  labels <- times_of_day[breaks]

  plot <-
    ggplot(data_summarized, mapping = aes(x = as.numeric(.data$starttime_of_day), y = .data$middle, group = NA)) +
    geom_line(aes(y = .data$ymin), lty = 2, color = ribbon_color) +
    geom_ribbon(aes(ymin = .data$lower, ymax = .data$upper), fill = ribbon_color, alpha = 0.5) +
    geom_line(aes(y = .data$middle2), lty = 2, color = middle_color) +
    geom_line(aes(y = .data$ymax), lty = 2, color = ribbon_color) +
    geom_line(color = middle_color) +
    lemon::facet_rep_grid(parameter~weekend, scales = "free_y", switch = "y") +
    scale_x_continuous(
#      limits = c(1, 24.5),
      breaks = breaks,
      labels = labels,
      expand = expand_scale(mult = expand_xscale)
    ) +
    theme_rop_diuarnal() +
    theme(
#      panel.spacing.x = unit(0, "lines"),
#      panel.grid.major.y = element_line(linetype = 2, size = 0.25, colour = "gray80"),
      axis.title = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  return(plot)
}



#' ggplot2-wrapper to summarise and plot data (of [rOstluft::format_rolf()] format) as boxplots
#' comparing weekdays to weekends
#'
#'
#' @param data a data.frame or tibble containing the data
#' @param y a character string specifying the taget variable to be summarised, usually "value".
#' outcomes of [rOstluft::cut_timeseries_periodic()].
#' @param fun function or list of functions for summary, can be named (then the outut stat is named after the function's name);
#'   Strings matching the regular expression `/^percentile([0-9]){1,2}$/i` are converted into the respective function.
#'   "percentile95" => `function(x, ...) quantile(x, 95 / 100, ...)`
#' @param ... further arguments passed on to [ggplot2::geom_boxplot()].
#'
#' @return [ggplot2::ggplot()] object
#' @export
#'
#' @examples
#' fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_2010-2014.csv")
#'
#' data <-
#'   rOstluft::read_airmo_csv(fn) %>%
#'   dplyr::filter(parameter %in% c("NOx", "O3", "PM10")) %>%
#'   rOstluft::resample(new_interval = "h1")
#'
#' ggweekend(data)
#'
#' # change y axis title using bquote
#' ggweekend(data) +
#'   ylab(bquote("Konzentration " ~ 0[3] ~ ", " ~ PM[10] ~ "(" ~ mu ~ "g " ~ m^{-3} ~ "), NOx (ppb)" ))
#'
#' # change fill scale
#' ggweekend(data) +
#'   scale_fill_brewer(type = "qual", palette = "Pastel1")
ggweekend <- function(
  data,
  y = "value",
  nmin = 3,
  fun = list(middle = "mean", middle2 = "median", ymin = "percentile05",
             lower = "percentile25", upper = "percentile75", ymax = "percentile95"),
  ...
) {

  groupings = grp("weekend")
  fun.args = list(na.rm = TRUE)

  data_summarized <- summary_periodic(data, y = y, groupings = groupings, fun = fun, fun.args = fun.args, nmin = nmin)
  data_summarized <- tidyr::spread(data_summarized, "stat", "value")

  mapping_plot <-  aes(
    x = .data$parameter,
    group = interaction(.data$weekend, .data$parameter),
    fill = .data$weekend
  )

  mapping_boxplot <- aes(
    ymin = .data$ymin,
    lower = .data$lower,
    middle = .data$middle,
    upper = .data$upper,
    ymax = .data$ymax
  )

  plot <-
    ggplot(data_summarized, mapping_plot) +
    geom_boxplot(mapping_boxplot, stat = "identity", outlier.colour = NA, ...) +
    geom_point(aes(y = .data$middle2), shape = 21, position = position_dodge(width = 0.9)) +
    scale_fill_discrete() +  # ordered factors use viridis as default see changelog 3.0.0
    theme_rop_diuarnal() +
    theme(
      strip.text = element_text(hjust = 0),
      legend.title = element_blank()
    ) + ylab("Konzentration")

  return(plot)
}


#' ggplot2-wrapper to summarise and plot data (of [rOstluft::format_rolf()] format) for one diurnal time course-stat
#' plotted as difference between weekdays - weekend
#'
#' @param data a data.frame or tibble containing the data
#' @param y a character string specifying the taget variable to be summarised, usually "value".
#' outcomes of [rOstluft::cut_timeseries_periodic()].
#' @param fun function or list of functions for summary, can be named (then the outut stat is named after the function's name);
#'   Strings matching the regular expression `/^percentile([0-9]){1,2}$/i` are converted into the respective function.
#'   "percentile95" => `function(x, ...) quantile(x, 95 / 100, ...)`
#' @param expand_xscale multiplicative range expansion factor
#' @param ... further arguments passed on to [ggplot2::geom_line()].
#'
#' @return [ggplot2::ggplot()] object
#' @export
#'
#' @examples
#' fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_2010-2014.csv")
#'
#' data <-
#'   rOstluft::read_airmo_csv(fn) %>%
#'   dplyr::filter(parameter %in% c("NOx", "O3", "PM10")) %>%
#'   rOstluft::resample(new_interval = "h1")
#'
#' ggdiurnal_weekend_diff(data)
ggdiurnal_weekend_diff <- function(
  data,
  y = "value",
  nmin = 3,
  fun = list(middle = "mean"),
  expand_xscale = 0.025,
  ...
) {

  groupings = grp("starttime_of_day", "weekend")
  fun.args = list(na.rm = TRUE)

  data_summarized <- summary_periodic(data, y = y, groupings = groupings, fun = fun, fun.args = fun.args, nmin = nmin)

  # extract the weekend levels and make symbols for ggplot mapping
  weekday <- rlang::sym(levels(data_summarized$weekend)[1])
  weekend <- rlang::sym(levels(data_summarized$weekend)[2])

  data_summarized <- tidyr::spread(data_summarized, "stat", "value")
  data_summarized <- dplyr::mutate(data_summarized,
    parameter = factor(stringr::str_c("Delta ", .data$parameter, " (", .data$unit, ")"))
  )

  data_summarized <-
    data_summarized %>%
    dplyr::select("starttime_of_day", "weekend", "site", "interval", "parameter", "middle") %>%
    tidyr::spread("weekend", "middle")


  times_of_day <- levels(data_summarized$starttime_of_day)
  if (length(times_of_day) < 24) {
    rlang::warn("creating diurnal diagramm with resolution greater then h1.")
  }

  breaks <- seq(length.out = 8, by = length(times_of_day) / 8 )
  labels <- times_of_day[breaks]

  plot <-
    ggplot(data_summarized, aes(x = as.numeric(.data$starttime_of_day), y = !!weekday - !!weekend, group = .data$parameter)) +
    geom_hline(yintercept = 0, lty = 2, color = "gray60") +
    geom_path(...) +
    lemon::facet_rep_grid(parameter~., scales = "free_y", switch = "y") +
    scale_x_continuous(
      breaks = breaks,
      labels = labels,
      expand = expand_scale(mult = expand_xscale)
    ) +
    theme_rop_diuarnal()

  return(plot)
}
