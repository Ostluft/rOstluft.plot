# require(ggplot2)
# require(lubridate)
# require(viridis)
# require(ggExtra)


#' Pads data to complete year
#' additional argument to fill some columns like:
#' https://github.com/davidcarslaw/openair/blob/592a076491791505b0ffcde099d84e7973f8b1c8/R/utilities.R#L78
#'
#' @param data input data
#' @param date_col column containing data
#' @param interval interval between two dates
#'
#' @return padded data
#' @export
pad_to_year <- function(data, date_col, interval) {
  date_col = ensym(date_col)

  dates <- dplyr::pull(data, !!date_col)
  start_date <- lubridate::floor_date(min(dates), "year")
  end_max <- max(dates)
  end_date <- lubridate::ceiling_date(end_max, "year")
  drop_last <- (end_max != end_date)

  dates <- seq(start_date, end_date, by = interval)

  if (isTRUE(drop_last)) {
    dates <- utils::head(dates, -1)
  }

  dates <- tibble::tibble( !!date_col := dates)

  dplyr::right_join(data, dates, by = rlang::as_name(date_col))
}



#' Plotting data on a calendar
#'
#' @description
#'
#' Calculated Columns:
#'
#' * monthday: day of month
#' * month: factor value = 1-12, level = localized abbreviated name
#' * year
#' * weekday: factor value = 1-7, level = localized name
#' * week
#' * x: year x 100 + week
#'
#' We need a continous x for placing breaks for every month on every facet
#'
#' @param data input data
#' @param x date column as Date, POSIXct or Character
#' @param z value columns
#' @param tile_opt geom_tile params. see `ggplot2::geom_tile()` and `ggplot2::layer()`
#' @param scale_fill scale for tile fill. Default is for continous data. For discrete data one must be provided
#' @param label expression for label or NULL
#' @param label_opt geom_text params. see `ggplot2::geom_text()` and `ggplot2::layer()`
#' @param border draw a border for every month. TRUE or FALSE
#' @param border_opt geom_segement params. see `ggplot2::geom_segment()` and `ggplot2::layer()`
#' @param marker expression for placing marker (`dplyr::filter()`) or NULL
#' @param marker_opt geom_point params. see `ggplot2::geom_segment()` and `ggplot2::layer()`
#' @param locale locale string. see `Sys.getlocale()`
#'
#' @return ggplot2 object
#' @export
plt_calendar <- function(data, x = "starttime", z = "value",
                         tile_opt = list(size = 0.1, color = "white"),
                         scale_fill = scale_fill_viridis_c(),
                         label = monthday,
                         label_opt = list(size = 2, color = "white"),
                         border = TRUE,
                         border_opt = list(size = 1, lineend = "square", linejoin = "bevel", color = "grey5"),
                         marker = NULL,
                         marker_opt = list(size = 2, position = position_nudge(y = 0.3), color = "white"),
                         locale = Sys.getlocale("LC_TIME")) {
  x <- ensym(x)
  z <- ensym(z)

  marker <- enexpr(marker)
  label <- enexpr(label)

  # parse dates if necessary
  data <- dplyr::mutate(data, date = lubridate::as_date(!!x))

  # pad to complete years (and sorts at the same time)
  data <- pad_to_year(data, "date", "1 day")

  # add needed variables
  data <- dplyr::mutate(data,
    monthday = lubridate::mday(.data$date),
    month = lubridate::month(.data$date, label = TRUE, locale = locale),
    year = lubridate::year(.data$date),
    weekday = lubridate::wday(.data$date, label = TRUE, week_start = 1, locale = locale),
    weekday = forcats::fct_rev(weekday),
    week = as.numeric(format(.data$date,"%W")),
    # we need a continuous x axis based on week of the year for placing x breaks
    x = .data$year * 100 + .data$week
  )

  breaks <- dplyr::filter(data, .data$monthday == 1)

  tile_pos <- purrr::pluck(tile_opt, "position", .default = "identity")
  tile_opt <- purrr::list_modify(tile_opt, position = NULL)

  p <-
    ggplot(data, aes(x = x, y = weekday, fill = !!z)) +
    layer("tile", "identity", NULL, NULL, tile_pos, tile_opt) +
    scale_fill +
    theme_minimal() +
    ggExtra::removeGridX() +
    labs(x = NULL, y = NULL) +
    theme(
      plot.title = element_text(hjust = 0),
      axis.ticks.x = element_line(),
      axis.text = element_text(size = 7),
      panel.spacing.x = unit(0.1, "lines"),
      strip.text = element_text(hjust = 0)
    ) +
    facet_wrap(~year, ncol = 1, scales = "free_x") +
    scale_x_continuous(
      expand = c(0.001,0),
      breaks = breaks$x,
      labels = breaks$month
    )


  if (isTRUE(border)) {
    # probably i'm just too stupid for an more elegant way, but it works
    data <- dplyr::mutate(data,
      weekday_num = as.numeric(.data$weekday),
      month_num = as.numeric(.data$month)
    )
    lines_left <- dplyr::filter(data, .data$monthday <= 7)
    lines_left <- dplyr::mutate(lines_left, xstart = .data$x - 0.5, xend = .data$x - 0.5,
                                ystart = .data$weekday_num - 0.5, yend = .data$weekday_num + 0.5)

    lines_right <- dplyr::filter(data, .data$month_num == 12 & .data$monthday >= 25)
    lines_right <- dplyr::mutate(lines_right, xstart = .data$x + 0.5, xend = .data$x + 0.5,
                                 ystart = .data$weekday_num - 0.5, yend = .data$weekday_num + 0.5)

    lines_top <- dplyr::filter(data, .data$monthday == 1 | data$weekday_num == 7 )
    lines_top <- dplyr::mutate(lines_top, xstart = .data$x - 0.5, xend = .data$x + 0.5,
                               ystart = .data$weekday_num + 0.5, yend = .data$weekday_num + 0.5)

    lines_bot <- dplyr::filter(data, data$weekday_num == 1 | (.data$monthday == 31 & .data$month_num == 12))
    lines_bot <- dplyr::mutate(lines_bot, xstart = .data$x - 0.5, xend = .data$x + 0.5,
                               ystart = .data$weekday_num - 0.5, yend = .data$weekday_num - 0.5)

    lines <- dplyr::bind_rows(lines_left, lines_right, lines_top, lines_bot)

    border_pos <- purrr::pluck(border_opt, "position", .default = "identity")
    border_opt <- purrr::list_modify(border_opt, position = NULL)
    p <- p + layer("segment", "identity", lines, aes(x=xstart, xend=xend, y=ystart, yend=yend),
                   border_pos, border_opt)
  }

  if (!is.null(label)) {
    label_pos <- purrr::pluck(label_opt, "position", .default = "identity")
    label_opt <- purrr::list_modify(label_opt, position = NULL, na.rm = TRUE)
    p <- p + layer("text", "identity", NULL, aes(label = !!label), label_pos, label_opt)
  }

  if (!is.null(marker)) {
    markers <- dplyr::filter(data, !!marker)
    marker_pos <- purrr::pluck(marker_opt, "position", .default = "identity")
    marker_opt <- purrr::list_modify(marker_opt, position = NULL)
    p <- p + layer("point", "identity", markers, NULL, marker_pos, marker_opt)
  }

  p
}



CalMonthBorder <- ggproto("CalMonthBorder", Stat,
  required_aes = c("x", "y", "month", "monthday"),   # y = weekday !!!
  compute_group = function(data, scales) {
    data <- dplyr::mutate(data, month = as.numeric(month))
    # we can't define the mapping for segment, so we need to use the default mapping: x, y, xend, yend
    # => mapping = column names

    # to the left of the tile @ the first 7 days of every month
    lines_left <- dplyr::filter(data, .data$monthday <= 7)
    lines_left <- dplyr::mutate(lines_left,
      xend = .data$x - 0.5,
      x = .data$x - 0.5,
      yend = .data$y + 0.5,
      y = .data$y - 0.5
    )
    # to the right of the tile @ the last week of december
    lines_right <- dplyr::filter(data, .data$month == 12 & .data$monthday >= 25)
    lines_right <- dplyr::mutate(lines_right,
      xend = .data$x + 0.5,
      x = .data$x + 0.5,
      yend = .data$y + 0.5,
      y = .data$y - 0.5
    )
    # above tile @ every first of month and monday
    lines_top <- dplyr::filter(data, .data$monthday == 1 | data$y == 7 )
    lines_top <- dplyr::mutate(lines_top,
      xend = .data$x + 0.5,
      x = .data$x - 0.5,
      yend = .data$y + 0.5,
      y = .data$y + 0.5
    )
    # below tile @ sunday and the 31. december
    lines_bot <- dplyr::filter(data, .data$y == 1 | (.data$monthday == 31 & .data$month == 12))
    lines_bot <- dplyr::mutate(lines_bot,
      xend = .data$x + 0.5,
      x = .data$x - 0.5,
      yend = .data$y - 0.5,
      y = .data$y - 0.5
    )

    dplyr::bind_rows(lines_left, lines_right, lines_top, lines_bot)
  }
)






#' Add month border to calendar
#'
#' @inheritParams ggplot2::geom_segment
#'
#' @return ggplot2 layer
#' @export
cal_month_border <- function(size = 1, lineend = "square", linejoin = "bevel", color = "grey5", ...) {
  # we use geom_segment to draw the month border
  # the Stat AddmonthBorder takes care of the calculations of x, y, xend, yend
  # it works, but is this the correct way?
  layer(
    stat = CalMonthBorder, data = NULL, mapping = NULL,
    geom = "segment", position = "identity",  show.legend = FALSE, inherit.aes = TRUE,
    params = list(size = size, lineend = lineend, linejoin = linejoin, color = color, ...)
  )
}



#' Adds Label to a calendar
#'
#' @inheritParams ggplot2::geom_text
#'
#' @return ggplot2 layer
#' @export
cal_label <- function(mapping = NULL, stat = "identity", data = NULL, geom = "text", position = "identity",
                      show.legend = FALSE, inherit.aes = TRUE, na.rm = TRUE, size = 2, color = "white", ...) {
  layer(
    stat = stat, data = data, mapping = mapping, geom = geom, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, size = size, color = color, ...)
  )
}

statFilter <- ggproto("statFilter", Stat,
  required_aes = c("filter"),

  compute_group = function(data, scales, ...)  {
    dplyr::filter(data, .data$filter == TRUE)
  }

)


#' Filtering data
#'
#' Removes values where the aesthetic filter evaluates to `FALSE`.
#'
#' @inheritParams ggplot2::stat_identity
#'
#' @return ggplot2 layer
#' @export
stat_filter <- function(mapping = NULL, data = NULL, geom = "point", position = "identity",
                        show.legend = FALSE, inherit.aes = TRUE, na.rm = TRUE, ...) {
  layer(
    stat = statFilter, data = data, mapping = mapping, geom = geom, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' calendar plot
#'
#' @param data input data
#' @param x date column as Date, POSIXct or Character
#' @param z value columns
#' @param scale_fill scale for tile fill. Default is for continous data. For discrete data one must be provided
#' @param size space between tiles
#' @param color color of space
#' @param ... more options for `ggplot2::geom_tile()`
#' @param locale locale string for `lubridate::month()` and `lubridate::wday()`. see `Sys.getlocale()`
#'
#' @return ggplot2::ggplot object
#' @export
plt_cal <- function(data, x = "starttime", z = "value",
                    size = 0.1, color = "white", ..., locale = Sys.getlocale("LC_TIME")) {
  x <- ensym(x)
  z <- ensym(z)

  # parse dates if necessary
  data <- dplyr::mutate(data, date = lubridate::as_date(!!x))

  # pad to complete years (and sorts at the same time)
  data <- pad_to_year(data, "date", "1 day")

  # add needed variables
  data <- dplyr::mutate(data,
    monthday = lubridate::mday(.data$date),
    month = lubridate::month(.data$date, label = TRUE, locale = locale),
    year = lubridate::year(.data$date),
    weekday = lubridate::wday(.data$date, label = TRUE, week_start = 1, locale = locale),
    weekday = forcats::fct_rev(weekday),
    week = as.numeric(format(.data$date,"%W")),
    # we need a continuous x axis based on week of the year for placing x breaks
    x = .data$year * 100 + .data$week
  )

  breaks <- dplyr::filter(data, .data$monthday == 1)

  # mapping for group, monthday, month is for month border
  # group = 1 disable grouping see ?ggplot2::aes_group_order
  ggplot(data, aes(x = x, y = weekday, fill = !!z,  monthday = monthday, month = month, group = 1)) +
    layer("tile", "identity", NULL, NULL, "identity", params = list(size = size, color = color, ...)) +
    theme_minimal() +
    ggExtra::removeGridX() +
    labs(x = NULL, y = NULL) +
    theme(
      plot.title = element_text(hjust = 0),
      axis.ticks.x = element_line(),
      axis.text = element_text(size = 7),
      panel.spacing.x = unit(0.1, "lines"),
      strip.text = element_text(hjust = 0)
    ) +
    facet_wrap(~year, ncol = 1, scales = "free_x") +
    scale_x_continuous(
      expand = c(0.001,0),
      breaks = breaks$x,
      labels = breaks$month
    )


}














ggcalendar <- function(df, x = "starttime", z = "value", fill_scale = scale_fill_viridis(discrete = TRUE, na.translate = FALSE), plot_dates = TRUE, date_label = "monthday", date_col = "white", date_size = 2) {

  df <- mutate(df, date = as.Date(!!sym(x)))
  df <-
    df %>%
    right_join(tibble(date = seq.Date(as.Date(paste0(min(year(df$date)),"-01-01")), as.Date(paste0(year(max(df$date)),"-12-31")), by = 1)), by = "date") %>%
    mutate(
      monthday = lubridate::mday(date),
      month = lubridate::month(date, label = TRUE),
      # month = as.yearmon(date),
      year = lubridate::year(date),
      dow = lubridate::wday(date, label = TRUE, week_start = 1),
      dow = factor(dow, levels = rev(levels(dow))),
      week = as.numeric(format(date,"%W"))
    ) %>%
    group_by(year) %>%
    mutate(
      week = week + ifelse(min(week) == 0, 1, 0),
      week2 = as.numeric(min(date)) + week - 1
    ) %>%
    group_by(year, month) %>%
    mutate(
      monthweek = 1 + week - min(week)
    ) %>%
    ungroup()
  breaks <- dplyr::filter(df, monthday == 1)

  p <-
    ggplot(df, aes_string(x = "week2", y = "dow", fill = z)) +
    geom_tile(colour = date_col, size = 0.1) +
    fill_scale +
    scale_x_continuous(
      expand = c(0,0),
      breaks = breaks$week2,
      labels = breaks$month
    ) +
    theme_minimal() +
    removeGridX() +
    labs(x = NULL, y = NULL)+
    theme(
      plot.title = element_text(hjust = 0),
      axis.ticks.x = element_line(),
      axis.text = element_text(size = 7),
      panel.spacing.x = unit(0.1, "lines"),
      strip.text = element_text(hjust = 0)
    ) +
    facet_wrap(~year, ncol = 1, scales = "free_x")

  if (plot_dates) {p <- p + geom_text(aes_string(label = date_label), size = date_size, color = date_col)}

  return(p)
}






ggcalendar2 <- function(df, x = "starttime", z = "value", fill_scale = scale_fill_viridis_c()) {
  p <-
    df %>%
    dplyr::mutate(date = lubridate::as_date(!!sym(x))) %>%
    ggplot(aes(y = 1, x = date, fill = !!sym(z))) +
    geom_raster() +
    theme_minimal() +
    fill_scale +
    facet_wrap(~lubridate::year(date), scales = "free_x", ncol = 1) +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_date(expand = c(0,0), date_breaks = "1 month", date_labels = "%b") +
    theme(
      panel.grid = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_line(),
      axis.title.x = element_blank(),
      strip.text = element_text(hjust = 0)
    )

  return(p)
}
