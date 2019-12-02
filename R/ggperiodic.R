
#' ggplot2-wrapper to summarise and plot data (of [rOstluft::format_rolf()] format) for diurnal time course-stats
#'
#' @param data a data.frame or tibble containing the data (wide format).
#' @param y a character string specifying the taget variable to be summarised, usually "value".
#' outcomes of [rOstluft::cut_timeseries_periodic()].
#' @param fun function or list of functions for summary, must be named , names have to iclude at least: middle, middle2, ymin, lower, upper, ymac;
#' pre-defined quantile-stat-functions can be directly included: "quantile01","quantile02", "quantile05", "quantile10", "quantile25",
#' "quantile75", "quantile90", "quantile95","quantile98","quantile99".
#' @param ribbon_color character string for specifying the ribbon color.
#' @param middle_color character string for specifying the middle's color.
#'
#' @return [ggplot2::ggplot()] object
#' @export
#'
#' @examples
#' library(rOstluft.plot)
#' library(rOstluft.data)
#' library(dplyr)
#' library(ggplot)
#' fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_2010-2014.csv")
#'
#' data <-
#'   rOstluft::read_airmo_csv(fn) %>%
#'   dplyr::filter(parameter %in% c("NOx", "O3", "PM10")) %>%
#'   rOstluft::resample(new_interval = "h1")
#'
#' ggdiurnal(data)
ggdiurnal <- function(

  data,
  y = "value",
  fun = list(middle = "mean", middle2 = "median", ymin = "quantile05",
             lower = "quantile25", upper = "quantile75", ymax = "quantile95"),
  nmin = 3,
  ribbon_color = "gray60",
  middle_color = "gray30"

) {

  groupings = grp(starttime_of_day)
  fun.args = list(na.rm = TRUE)

  data_summarized <- summary_periodic(data, y = y, groupings = groupings, fun = fun, fun.args = fun.args, nmin = nmin)
  data_summarized <- tidyr::spread(data_summarized, stat, value)
  data_summarized <- dplyr::mutate(data_summarized, parameter = factor(paste0(parameter, " (", unit, ")")))

  plot <-
    ggplot(data_summarized, mapping = aes(x = as.numeric(starttime_of_day) + 0.5, y = middle, group = NA)) +
    geom_line(aes(y = ymin), lty = 2, color = ribbon_color) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = ribbon_color, alpha = 0.5) +
    geom_line(aes(y = middle2), lty = 2, color = middle_color) +
    geom_line(aes(y = ymax), lty = 2, color = ribbon_color) +
    geom_line(color = middle_color) +
    lemon::facet_rep_grid(parameter~., scales = "free_y", switch = "y") + # ncol = ceiling(length(unique(data_summarized$parameter)) / 4)) +
    scale_x_continuous(breaks = seq(1, length(unique(data_summarized$starttime_of_day)) - 1, 3),
                       labels = unique(data_summarized$starttime_of_day)[seq(1, length(unique(data_summarized$starttime_of_day)) - 1, 3)],
                       expand = c(0.025,0.025)) +
    theme_rop_classic() +
    theme(
      axis.title = element_blank()
    )

  return(plot)
}








#' ggplot2-wrapper to summarise and plot data (of [rOstluft::format_rolf()] format) for diurnal time course-stats
#' including weekend-effect
#'
#' @param data a data.frame or tibble containing the data (wide format).
#' @param y a character string specifying the taget variable to be summarised, usually "value".
#' outcomes of [rOstluft::cut_timeseries_periodic()].
#' @param fun function or list of functions for summary, must be named , names have to iclude at least: middle, middle2, ymin, lower, upper, ymac;
#' pre-defined quantile-stat-functions can be directly included: "quantile01","quantile02", "quantile05", "quantile10", "quantile25",
#' "quantile75", "quantile90", "quantile95","quantile98","quantile99".
#' @param ribbon_color character string for specifying the ribbon color.
#' @param middle_color character string for specifying the middle's color.
#'
#' @return [ggplot2::ggplot()] object
#' @export
#'
#' @examples
#' library(rOstluft.plot)
#' library(rOstluft.data)
#' library(dplyr)
#' library(ggplot)
#' fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_2010-2014.csv")
#'
#' data <-
#'   rOstluft::read_airmo_csv(fn) %>%
#'   dplyr::filter(parameter %in% c("NOx", "O3", "PM10")) %>%
#'   rOstluft::resample(new_interval = "h1")
#'
#' ggdiurnal_weekend(data)
ggdiurnal_weekend <- function(

  data,
  y = "value",
  nmin = 3,
  fun = list(middle = "mean", middle2 = "median", ymin = "quantile05",
             lower = "quantile25", upper = "quantile75", ymax = "quantile95"),
  ribbon_color = "gray60",
  middle_color = "gray30"

) {

  groupings = grp(starttime_of_day, weekend)
  fun.args = list(na.rm = TRUE)

  data_summarized <- summary_periodic(data, y = y, groupings = groupings, fun = fun, fun.args = fun.args, nmin = nmin)
  data_summarized <- tidyr::spread(data_summarized, stat, value)
  data_summarized <- dplyr::mutate(data_summarized, parameter = factor(paste0(parameter, " (", unit, ")")))

  plot <-
    ggplot(data_summarized, mapping = aes(x = as.numeric(starttime_of_day) + 0.5, y = middle, group = NA)) +
    geom_line(aes(y = ymin), lty = 2, color = ribbon_color) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = ribbon_color, alpha = 0.5) +
    geom_line(aes(y = middle2), lty = 2, color = middle_color) +
    geom_line(aes(y = ymax), lty = 2, color = ribbon_color) +
    geom_line(color = middle_color) +
    lemon::facet_rep_grid(parameter~weekend, scales = "free_y", switch = "y") +
    scale_x_continuous(limits = c(1, 23.5), breaks = seq(1, length(unique(data_summarized$starttime_of_day)) - 1, 6),
                       labels = unique(data_summarized$starttime_of_day)[seq(1, length(unique(data_summarized$starttime_of_day)) - 1, 6)],
                       expand = c(0,0)) +
    theme_rop_classic() +
    theme(
      panel.spacing = unit(0, "lines"),
      panel.grid.major.y = element_line(linetype = 2, size = 0.25, colour = "gray80"),
      axis.title = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  return(plot)
}









#' ggplot2-wrapper to summarise and plot data (of [rOstluft::format_rolf()] format) as boxplots
#' comparing weekdays to weekends
#'
#'
#' @param data a data.frame or tibble containing the data (wide format).
#' @param y a character string specifying the taget variable to be summarised, usually "value".
#' outcomes of [rOstluft::cut_timeseries_periodic()].
#' @param fun function or list of functions for summary, must be named , names have to iclude at least: middle, middle2, ymin, lower, upper, ymac;
#' pre-defined quantile-stat-functions can be directly included: "quantile01","quantile02", "quantile05", "quantile10", "quantile25",
#' "quantile75", "quantile90", "quantile95","quantile98","quantile99".
#' @param ... further arguments passed on to [ggplot2::geom_boxplot()].
#'
#' @return [ggplot2::ggplot()] object
#' @export
#'
#' @examples
#' library(rOstluft.plot)
#' library(rOstluft.data)
#' library(dplyr)
#' library(ggplot)
#' fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_2010-2014.csv")
#'
#' data <-
#'   rOstluft::read_airmo_csv(fn) %>%
#'   dplyr::filter(parameter %in% c("NOx", "O3", "PM10")) %>%
#'   rOstluft::resample(new_interval = "h1")
#'
#' ggweekend(data)
ggweekend <- function(

  data,
  y = "value",
  nmin = 3,
  fun = list(middle = "median", middle2 = "mean", ymin = "quantile05",
             lower = "quantile25", upper = "quantile75", ymax = "quantile95"),
  ...

) {

  groupings = grp(weekend)
  fun.args = list(na.rm = TRUE)

  data_summarized <- summary_periodic(data, y = y, groupings = groupings, fun = fun, fun.args = fun.args, nmin = nmin)
  data_summarized <- tidyr::spread(data_summarized, stat, value)

  plot <-
    ggplot(data_summarized, mapping = aes(x = parameter, group = interaction(weekend, parameter), fill = weekend)) +
    geom_boxplot(mapping = aes(ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax),
                 stat = "identity", outlier.colour = NA, ...) +
    geom_point(aes(y = middle2), shape = 21, position = position_dodge(width = 0.9)) +
    theme_rop_classic() +
    theme(
      strip.text = element_text(hjust = 0),
      legend.title = element_blank()
    ) +
    ylab("value")

  return(plot)
}







#' ggplot2-wrapper to summarise and plot data (of [rOstluft::format_rolf()] format) for one diurnal time course-stat
#' plotted as difference between weekdays - weekend
#'
#' @param data a data.frame or tibble containing the data (wide format).
#' @param y a character string specifying the taget variable to be summarised, usually "value".
#' outcomes of [rOstluft::cut_timeseries_periodic()].
#' @param fun function or list of functions for summary, must be named, names have to iclude at least: middle;
#' pre-defined quantile-stat-functions can be directly included: "quantile01","quantile02", "quantile05", "quantile10", "quantile25",
#' "quantile75", "quantile90", "quantile95","quantile98","quantile99".
#' @param ... further arguments passed on to [ggplot2::geom_line()].
#'
#' @return [ggplot2::ggplot()] object
#' @export
#'
#' @examples
#' library(rOstluft.plot)
#' library(rOstluft.data)
#' library(dplyr)
#' library(ggplot)
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
  ...

) {

  groupings = grp(starttime_of_day, weekend)
  fun.args = list(na.rm = TRUE)

  data_summarized <- summary_periodic(data, y = y, groupings = groupings, fun = fun, fun.args = fun.args, nmin = nmin)
  data_summarized <- tidyr::spread(data_summarized, stat, value)
  data_summarized <- dplyr::mutate(data_summarized, parameter = factor(paste0("Delta ", parameter, " (", unit, ")")))
  data_summarized <-
    data_summarized %>%
    dplyr::select(starttime_of_day, weekend, site, interval, parameter, middle) %>%
    tidyr::spread(weekend, middle)

  plot <-
    ggplot(data_summarized, mapping = aes(x = as.numeric(starttime_of_day), y = weekday - weekend, group = parameter)) +
    geom_hline(yintercept = 0, lty = 2, color = "gray60") +
    geom_path(...) +
    lemon::facet_rep_grid(parameter~., scales = "free_y", switch = "y") +
    scale_x_continuous(breaks = seq(1, length(unique(data_summarized$starttime_of_day)) - 1, 3),
                       labels = unique(data_summarized$starttime_of_day)[seq(1, length(unique(data_summarized$starttime_of_day)) - 1, 3)],
                       expand = c(0.025,0.025)) +
    theme_rop_classic()

  return(plot)
}
