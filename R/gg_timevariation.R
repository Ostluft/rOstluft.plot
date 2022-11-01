#' Plot time variations
#'
#' @description
#' Inspired by [openair::timeVariation()] multiple functions to plot diurnal,
#' day hour of the week, day of the week and monthly variation. The functions are
#' optimized for data in long format and the defaults are for the rolf format from
#' the [rOstluft Package](https://ostluft.github.io/rOstluft/index.html).
#'
#'
#' @param data data frame with input data as hourly time series
#' @param dt a string or symbol for the date time column (default: starttime)
#' @param y a string or symbol specifying the target column to be summarised (default: value)
#' @param group column as string to be used to split the input data in
#'   multiple groups. Should be a member of `id_cols` eg. parameter or site (default NULL)
#' @param id_cols A set of columns that uniquely identifies each observation.
#'   Use `rOstluft.plot::grp()` for quoting.
#'   default `rOstluft.plot::grp(site, parameter, interval, unit)`
#' @param statistic  Can be `“mean”` (default) or `“median”`. If the statistic is ‘mean’ then
#'   the mean line and the 95% confidence interval in the mean are plotted by default.
#'   If the statistic is ‘median’ then the median line is plotted together with the
#'   25/75th quantiles are plotted. Users can control the confidence intervals with
#'   `draw_ci` and `conf_interval`
#' @param draw_ci if `TRUE` draw confidence interval or quantiles
#' @param conf_interval if `statistic = "mean"` value of confidence interval used as
#'   `conf.level` in `[stats::t.test()]`. If `statistic = "median"` the quantiles of
#'   `conv_interval` and `1 - conv_interval` are drawn.
#' @param ylab provide a custom y plot label
#' @param ylim limits for y scale see [ggplot2::scale_y_continuous()] for more infos.
#' @param legend_title provide a legend title
#' @param language_code ISO country code for the language used as weekdays and months
#'   labels (default: "de")
#'
#' @return a [ggplot2::ggplot()] object or in case of `gg_timevariaton()` a [patchwork::patchwork]  object
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_2010-2014.csv")
#'
#' data <-
#'   rOstluft::read_airmo_csv(fn) %>%
#'   rOstluft::pluck_parameter("NOx", "NO", "NO2") %>%
#'   rOstluft::resample(new_interval = "h1")
#'
#' # monthly variation of data
#' gg_timevariation_month(data, group = "parameter", ylab = "Konzentration [µg/m3]")
#'
#' # for faceting the variable must be included in the id_cols
#' gg_timevariation_wday(
#'   data,
#'   group = "parameter",
#'   id_cols = grp(site, parameter, interval, unit, season = cut_season(starttime))
#' ) + facet_wrap(vars(season))
#'
#' # utility function to compose all plots together using patchwork
#' # for advanced use cases you should probably compose the plot yourself
#' # you can use ylim to start all y axis by 0
#' gg_timevariation(
#'   data,
#'   group = "parameter",
#'   ylab = "Konzentration [µg/m3]",
#'   ylim = c(0, NA)
#' )
#'
#'
#' # you can use wide data, but only with a single pollutant
#' data_wide <- rOstluft::rolf_to_openair(data)
#'
#' gg_timevariation_wday_hour(
#'   data_wide,
#'   dt = date,
#'   y = NO2,
#'   group = "site",
#'   id_cols = grp(site)
#' )
#'
#' # you can also use a function in id_cols to create groups
#' gg_timevariation_diurnal(
#'   data_wide,
#'   dt = date,
#'   y = NO2,
#'   group = "season",
#'   id_cols = grp(site, season = cut_season(date))
#' )

gg_timevariation <- function(
  data,
  dt = "starttime",
  y = "value",
  group = NULL,
  id_cols = grp("site", "parameter", "interval", "unit"),
  statistic = c("mean", "median"),
  draw_ci = TRUE,
  conf_interval = NULL,
  ylab = ggplot2::waiver(),
  ylim = c(NA, NA),
  legend_title = NULL,
  language_code = "de"
) {
  p1 <- gg_timevariation_wday_hour(
    data = data,
    dt = {{dt}},
    y = {{y}},
    group = {{group}},
    id_cols = {{id_cols}},
    statistic = statistic,
    draw_ci = draw_ci,
    conf_interval = conf_interval,
    ylab = ylab,
    ylim = ylim,
    legend_title = legend_title,
    language_code = language_code
  )

  p2 <- gg_timevariation_diurnal(
    data = data,
    dt = {{dt}},
    y = {{y}},
    group = {{group}},
    id_cols = {{id_cols}},
    statistic = statistic,
    draw_ci = draw_ci,
    conf_interval = conf_interval,
    ylab = ylab,
    ylim = ylim,
    legend_title = legend_title,
    language_code = language_code
  ) + theme(legend.position = "none")

  p3 <- gg_timevariation_month(
    data = data,
    dt = {{dt}},
    y = {{y}},
    group = {{group}},
    id_cols = {{id_cols}},
    statistic = statistic,
    draw_ci = draw_ci,
    conf_interval = conf_interval,
    ylab = ylab,
    ylim = ylim,
    legend_title = legend_title,
    language_code = language_code
  ) +
    theme(legend.position = "none") +
    guides(x = guide_axis(check.overlap = TRUE))

  p4 <- gg_timevariation_wday(
    data = data,
    dt = {{dt}},
    y = {{y}},
    group = {{group}},
    id_cols = {{id_cols}},
    statistic = statistic,
    draw_ci = draw_ci,
    conf_interval = conf_interval,
    ylab = ylab,
    ylim = ylim,
    legend_title = legend_title,
    language_code = language_code
  ) + theme(legend.position = "none")

  design <- "111
             234"

  pp <- patchwork::wrap_plots(p1, p2, p3, p4, design = design)

  pp
}






#' @export
#' @rdname gg_timevariation
gg_timevariation_wday_hour <- function(
  data,
  dt = "starttime",
  y = "value",
  group = NULL,
  id_cols = grp("site", "parameter", "interval", "unit"),
  statistic = c("mean", "median"),
  draw_ci = TRUE,
  conf_interval = NULL,
  ylab = ggplot2::waiver(),
  ylim = c(NA, NA),
  legend_title = NULL,
  language_code = "de"
) {
  dt <- rlang::ensym(dt)
  y <- rlang::ensym(y)

  if (is.null(group)) {
    group <- y
  } else {
    group <- rlang::ensym(group)
  }

  statistic <- match.arg(statistic)

  if (is.null(conf_interval)) {
    if (statistic == "mean") {
      conf_interval = 0.95
    } else {
      conf_interval = 0.25
    }
  }

  data <- dplyr::mutate(data,
    wday = clock::weekday_code(clock::as_weekday(!!dt), encoding = "iso"),
    hour = clock::get_hour(!!dt),
    wday_hour = (.data$wday-1)*24 + .data$hour,
  )

  data <- dplyr::group_by(data, !!!id_cols, .data$wday_hour)

  if (statistic == "mean") {
    data_summarized <- dplyr::summarise(data,
      ci = broom::tidy(stats::t.test(!!y, conf.level = conf_interval)),
      !!y := mean(!!y, na.rm = TRUE),
      .groups = "drop"
    )
    data_summarized <- tidyr::unnest(data_summarized, .data$ci, names_sep = "#")
  } else {
    data_summarized <- dplyr::summarise(data,
      `ci#conf.low` = stats::quantile(!!y, probs = conf_interval, na.rm = TRUE),
      `ci#conf.high` = stats::quantile(!!y, probs = 1 - conf_interval, na.rm = TRUE),
      !!y := stats::median(!!y, na.rm = TRUE),
      .groups = "drop"
    )
  }

  # helper function
  weekday_int_to_string <- function(.x) {
    labs <- clock::weekday_factor(
      x = clock::weekday(.x %/% 24 + 1, encoding = "iso"),
      labels = language_code,
      abbreviate = FALSE,
      encoding = "iso"
    )
    labs
  }

  p <- ggplot(data_summarized, aes(
      x = .data$wday_hour,
      y = !!y,
      ymin = .data$`ci#conf.low`,
      ymax = .data$`ci#conf.high`,
      color = !!group,
      fill = !!group,
      group = !!group
    )) +
    geom_line(size = 1)


  if (isTRUE(draw_ci)) {
    message(glue::glue("plotting with statistic {statistic} and confidence interval of {conf_interval}"))
    p <- p + geom_ribbon(alpha=0.2)
  }

  p <- p +
    geom_vline(xintercept = 1:6 * 24) +
    scale_x_continuous(
      limits = c(0, 7*24-1),
      breaks = seq.int(0, 7*24 - 1, by = 6),
      labels = ~ . %% 24,
      expand = expansion(),
      sec.axis = sec_axis(
        trans = ~. ,
        breaks = seq.int(12, 7*24, by = 24),
        labels = weekday_int_to_string
      )
    ) +
    scale_y_continuous(limits = ylim) +
    theme_minimal() +
    theme(
      legend.position = "top"
    ) +
    labs(
      fill = legend_title,
      color = legend_title,
      y = ylab,
      x = "Tagesstunde"
    )

  p
}




#' @export
#' @rdname gg_timevariation
gg_timevariation_wday <- function(
  data,
  dt = "starttime",
  y = "value",
  group = NULL,
  id_cols = grp("site", "parameter", "interval", "unit"),
  statistic = c("mean", "median"),
  draw_ci = TRUE,
  conf_interval = NULL,
  ylab = ggplot2::waiver(),
  ylim = c(NA, NA),
  legend_title = NULL,
  language_code = "de"
) {
  dt <- rlang::ensym(dt)
  y <- rlang::ensym(y)

  if (is.null(group)) {
    group <- y
  } else {
    group <- rlang::ensym(group)
  }

  statistic <- match.arg(statistic)

  if (is.null(conf_interval)) {
    if (statistic == "mean") {
      conf_interval = 0.95
    } else {
      conf_interval = 0.25
    }
  }

  data <- dplyr::mutate(data,
    wday = clock::date_weekday_factor(!!dt, labels = language_code, encoding = "iso")
  )

  data <- dplyr::group_by(data, !!!id_cols, .data$wday)

  if (statistic == "mean") {
    data_summarized <- dplyr::summarise(data,
      ci = broom::tidy(stats::t.test(!!y, conf.level = conf_interval)),
      !!y := mean(!!y, na.rm = TRUE),
      .groups = "drop"
    )
    data_summarized <- tidyr::unnest(data_summarized, .data$ci, names_sep = "#")
  } else {
    data_summarized <- dplyr::summarise(data,
      `ci#conf.low` = stats::quantile(!!y, probs = conf_interval, na.rm = TRUE),
      `ci#conf.high` = stats::quantile(!!y, probs = 1 - conf_interval, na.rm = TRUE),
      !!y := stats::median(!!y, na.rm = TRUE),
      .groups = "drop"
    )
  }

  p <- ggplot(data_summarized, aes(
      x = .data$wday,
      y = !!y,
      ymin = .data$`ci#conf.low`,
      ymax = .data$`ci#conf.high`,
      color = !!group,
      fill = !!group,
      group = !!group
    )) +
    geom_line(size = 1)


  if (isTRUE(draw_ci)) {
    message(glue::glue("plotting with statistic {statistic} and confidence interval of {conf_interval}"))
    p <- p + geom_ribbon(alpha=0.2)
  }

  p <- p +
    scale_x_discrete(
      expand = expansion()
    ) +
    scale_y_continuous(limits = ylim) +
    theme_minimal() +
    theme(
      legend.position = "top"
    ) +
    labs(
      fill = legend_title,
      color = legend_title,
      y = ylab,
      x = "Wochentag"
    )

  p
}





#' @export
#' @rdname gg_timevariation
gg_timevariation_month <- function(
  data,
  dt = "starttime",
  y = "value",
  group = NULL,
  id_cols = grp("site", "parameter", "interval", "unit"),
  statistic = c("mean", "median"),
  draw_ci = TRUE,
  conf_interval = NULL,
  ylab = ggplot2::waiver(),
  ylim = c(NA, NA),
  legend_title = NULL,
  language_code = "de"
) {
  dt <- rlang::ensym(dt)
  y <- rlang::ensym(y)

  if (is.null(group)) {
    group <- y
  } else {
    group <- rlang::ensym(group)
  }

  statistic <- match.arg(statistic)

  if (is.null(conf_interval)) {
    if (statistic == "mean") {
      conf_interval = 0.95
    } else {
      conf_interval = 0.25
    }
  }

  data <- dplyr::mutate(data,
    month = clock::date_month_factor(!!dt, labels = language_code, abbreviate = TRUE)
  )

  data <- dplyr::group_by(data, !!!id_cols, .data$month)

  if (statistic == "mean") {
    data_summarized <- dplyr::summarise(data,
      ci = broom::tidy(stats::t.test(!!y, conf.level = conf_interval)),
      !!y := mean(!!y, na.rm = TRUE),
      .groups = "drop"
    )
    data_summarized <- tidyr::unnest(data_summarized, .data$ci, names_sep = "#")
  } else {
    data_summarized <- dplyr::summarise(data,
      `ci#conf.low` = stats::quantile(!!y, probs = conf_interval, na.rm = TRUE),
      `ci#conf.high` = stats::quantile(!!y, probs = 1 - conf_interval, na.rm = TRUE),
      !!y := stats::median(!!y, na.rm = TRUE),
      .groups = "drop"
    )
  }

  p <- ggplot(data_summarized, aes(
      x = .data$month,
      y = !!y,
      ymin = .data$`ci#conf.low`,
      ymax = .data$`ci#conf.high`,
      color = !!group,
      fill = !!group,
      group = !!group
    )) +
    geom_line(size = 1)


  if (isTRUE(draw_ci)) {
    message(glue::glue("plotting with statistic {statistic} and confidence interval of {conf_interval}"))
    p <- p + geom_ribbon(alpha=0.2)
  }

  p <- p +
    scale_x_discrete(
      expand = expansion()
    ) +
    scale_y_continuous(limits = ylim) +
    theme_minimal() +
    theme(
      legend.position = "top"
    ) +
    labs(
      fill = legend_title,
      color = legend_title,
      y = ylab,
      x = "Monat"
    )

  p
}



#' @export
#' @rdname gg_timevariation
gg_timevariation_diurnal <- function(
  data,
  dt = "starttime",
  y = "value",
  group = NULL,
  id_cols = grp("site", "parameter", "interval", "unit"),
  statistic = c("mean", "median"),
  draw_ci = TRUE,
  conf_interval = NULL,
  ylab = ggplot2::waiver(),
  ylim = c(NA, NA),
  legend_title = NULL,
  language_code = "de"
) {
  dt <- rlang::ensym(dt)
  y <- rlang::ensym(y)

  if (is.null(group)) {
    group <- y
  } else {
    group <- rlang::ensym(group)
  }

  statistic <- match.arg(statistic)

  if (is.null(conf_interval)) {
    if (statistic == "mean") {
      conf_interval = 0.95
    } else {
      conf_interval = 0.25
    }
  }

  data <- dplyr::mutate(data,
    hour = clock::get_hour(!!dt)
  )

  data <- dplyr::group_by(data, !!!id_cols, .data$hour)

  if (statistic == "mean") {
    data_summarized <- dplyr::summarise(data,
      ci = broom::tidy(stats::t.test(!!y, conf.level = conf_interval)),
      !!y := mean(!!y, na.rm = TRUE),
      .groups = "drop"
    )
    data_summarized <- tidyr::unnest(data_summarized, .data$ci, names_sep = "#")
  } else {
    data_summarized <- dplyr::summarise(data,
      `ci#conf.low` = stats::quantile(!!y, probs = conf_interval, na.rm = TRUE),
      `ci#conf.high` = stats::quantile(!!y, probs = 1 - conf_interval, na.rm = TRUE),
      !!y := stats::median(!!y, na.rm = TRUE),
      .groups = "drop"
    )
  }

  p <- ggplot(data_summarized, aes(
      x = .data$hour,
      y = !!y,
      ymin = .data$`ci#conf.low`,
      ymax = .data$`ci#conf.high`,
      color = !!group,
      fill = !!group,
      group = !!group
    )) +
    geom_line(size = 1)


  if (isTRUE(draw_ci)) {
    message(glue::glue("plotting with statistic {statistic} and confidence interval of {conf_interval}"))
    p <- p + geom_ribbon(alpha=0.2)
  }

  p <- p +
    scale_x_continuous(
      breaks = seq(0, 24, by = 6),
      expand = expansion()
    ) +
    scale_y_continuous(limits = ylim) +
    theme_minimal() +
    theme(
      legend.position = "top"
    ) +
    labs(
      fill = legend_title,
      color = legend_title,
      y = ylab,
      x = "Tagesstunde"
    )

  p
}

