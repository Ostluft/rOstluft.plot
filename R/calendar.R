#' calendar plot
#'
#' TODO: example
#'
#' @param data input data
#' @param x date column as Date, POSIXct or Character
#' @param z value columns
#' @param size space between tiles
#' @param color color of space
#' @param ... more options for `ggplot2::geom_tile()`
#' @param locale locale string for `lubridate::month()` and `lubridate::wday()`. see `Sys.getlocale()`
#'
#' @return ggplot2 object
#' @export
plt_cal <- function(data, x = "date", z = "O3_max_h1",
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
  ggplot(data, aes(x = x, y = weekday, fill = !!z)) +
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


#' @rdname rOstluft.plot-ggproto
#' @usage NULL
#' @format NULL
#' @export
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


#' Adds month border to calendar
#'
#' @inheritParams ggplot2::geom_segment
#'
#' @return ggplot2 layer
#'
#' @rdname plt_cal
#' @export
cal_month_border <- function(size = 1, lineend = "square", linejoin = "bevel", color = "grey5", ...) {
  # we use geom_segment to draw the month border
  # the Stat AddmonthBorder takes care of the calculations of x, y, xend, yend
  # it works, but is this the correct way?
  layer(
    stat = CalMonthBorder, data = NULL, mapping = aes(month = month, monthday = monthday),
    geom = "segment", position = "identity",  show.legend = FALSE, inherit.aes = TRUE,
    params = list(size = size, lineend = lineend, linejoin = linejoin, color = color, ...)
  )
}


#' Adds Label to a calendar
#'
#' Just a geom_text with some defaults
#'
#' @inheritParams ggplot2::geom_text
#' @param geom geom used for layer. "label" could be an alternative
#'
#' @return ggplot2 layer
#'
#' @rdname plt_cal
#' @export
cal_label <- function(mapping = NULL, stat = "identity", data = NULL, geom = "text", position = "identity",
                      show.legend = FALSE, inherit.aes = TRUE, na.rm = TRUE, size = 2, color = "white", ...) {
  layer(
    stat = stat, data = data, mapping = mapping, geom = geom, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, size = size, color = color, ...)
  )
}

