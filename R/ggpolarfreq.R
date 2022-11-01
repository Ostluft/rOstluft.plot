#' @title ggplot2 wrapper to to plot wind speed/direction frequencies and other statistics
#'
#' @description
#' Using [summary_wind()] to calculate und plot summary statisitics over wind direction and
#' wind velocity bins. Primarily plots wind speed/direction frequencies. Each bin is colour-coded
#' depending on the frequency of measurements. Bins can also be used to show the concentration of
#' pollutants using a range of commonly used statistics.
#'
#' @param data a data.frame or tibble containing the data (wide format)
#' @param ws NULL or symbol giving the wind velocity parameter name (wind velocity preferably in m/s)
#' @param wd symbol giving the wind direction parameter name
#' @param z symbol giving the parameter name to be summarised
#' @param wd_binwidth width of the wind direction bins in degrees wind  direction
#'   (must fullfill `binwidth %in% 360 / c(4, 8, 16, 32)`)
#' @param ws_binwidth width of the wind speed bins
#' @param ws_max if ws_max is specified, a bin with >ws_max will be included
#' @param groupings additional groupings. Use helper [grp()] to create
#' @param fun function or list of functions for summary.
#' @param fun.args a list of extra arguments passed on to fun.
#' @param nmin numeric, minimum number of values for fun, if n < nmin: NA is returned
#' @param fill_scale ggplot2 discrete fill scale, e.g. [ggplot2::scale_fill_gradientn()]
#' @param bg bg raster map, e.g. ggmap object as plot background
#' @param calm if specified as numeric all windspeed below this value will be treated
#'   as calm and the proportion will be displayed as text in the center. The color, size of
#'   the text can be supplied with `calm.size` and `calm.color`. A prefix for the text
#'   can be defined with `calm.prefix` and the accuracy of the number with `calm.accuracy`
#' @param calm.prefix prefix for the calm text as string. Supplied to [scales::label_percent()] as
#'  argument.
#' @param calm.accuracy A number to round to. Use (e.g.) 0.01 to show 2 decimal places of
#'   precision. If NULL uses a heuristic that should ensure breaks have the minimum number
#'   of digits needed to show the difference between adjacent values.
#' @param calm.color text color
#' @param calm.size text size
#' @param yexpand size of the empty calm circle in the center as percentage of the y scale
#' @param ... Other arguments passed on to [ggplot2::geom_bar()]. Used
#'   to set an aesthetic to a fixed value. Defaults are `color = "white", width = 1, size = 0.25`
#'
#' @return [ggplot2::ggplot()] object
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' fn <- rOstluft.data::f("Zch_Schimmelstrasse_2010-2014.csv")
#'
#' data <-
#'   rOstluft::read_airmo_csv(fn) %>%
#'   rOstluft::rolf_to_openair() %>%
#'   openair::cutData(type = "season")
#'
#' # data with empty sectors
#' npoints <- 1000
#' data2 <- tibble::tibble(
#'   wd = runif(npoints, 90, 315),
#'   ws = runif(npoints, 0, 6)
#' )
#'
#' # more bin, wider bars
#' ggpolarfreq(data2, ws, wd, wd_binwidth = 22.5, width = 1)
#'
#' # squish wind speed
#' ggpolarfreq(data2, ws, wd, ws_max = 4)
#'
#' # add a lower limit for wind speed (calm)
#' ggpolarfreq(data2, ws, wd, ws_max = 4, calm = 0.5)
#'
#' # with real data
#' ggpolarfreq(data, ws, wd, ws_max = 2, ws_binwidth = 0.5)
#'
#' # use grp for faceting
#' ggpolarfreq(data, ws, wd, ws_max = 2, ws_binwidth = 0.5, groupings = grp(season)) +
#'   facet_wrap(vars(season))
#'
#' # show the frequency of a pollutant measurement
#' ggpolarfreq(
#'   data = data,
#'   ws = ws,
#'   wd = wd,
#'   z = "NOx",
#'   ws_max = 2,
#'   ws_binwidth = 0.5,
#' ) + labs(title = "Frequency of NOx Measurements")
#'
#' # use z and provide a stat function, also provide fill_scale for
#' # correct formating
#' ggpolarfreq(
#'   data = data,
#'   ws = ws,
#'   wd = wd,
#'   z = "NOx",
#'   fun = "mean",
#'   fun.args = list(na.rm = TRUE),
#'   ws_max = 2,
#'   ws_binwidth = 0.5,
#'   fill_scale = scale_fill_viridis_c()
#' ) + labs(title = "Mean value of NOx")
#'
#' ggpolarfreq(
#'   data = data,
#'   ws = ws,
#'   wd = wd,
#'   z = "NOx",
#'   fun = ~quantile(., probs = 0.95),
#'   fun.args = list(na.rm = TRUE),
#'   ws_max = 2,
#'   ws_binwidth = 0.5,
#'   fill_scale = scale_fill_viridis_c()
#' ) + labs(title = "95% Percentile of NOx")
ggpolarfreq <- function(
  data, ws, wd,
  z = NULL,
  wd_binwidth = 45,
  ws_binwidth = 1,
  ws_max = NA,
  groupings = grp(),
  fun = "frequency",
  fun.args = list(),
  nmin = 1,
  fill_scale = scale_fill_viridis_c(labels = scales::percent),
  bg = NULL,
  calm = NA,
  calm.prefix = "Calm\n",
  calm.accuracy = 1,
  calm.color = "black",
  calm.size = 3.88,
  yexpand = 0.2,
  ...
) {

  ws <- rlang::ensym(ws)  # or enquo but summary_wind accept only strings or symbols
  wd <- rlang::ensym(wd)
  if (is.null(z)) {
    z <- rlang::ensym(ws)
  } else {
    z <- rlang::ensym(z)
  }

  if (fun == "frequency") {
    fill <- rlang::sym("freq")
  } else {
    fill <- z
  }

  wd_cutfun <- cut_wd.fun(binwidth = wd_binwidth)
  ws_cutfun <- cut_ws.fun(binwidth = ws_binwidth, ws_max = ws_max, calm = calm)

  data_summarized <- summary_wind(
    data = data,
    ws = !!ws,
    wd = !!wd,
    z = !!z,
    groupings = groupings,
    fun = fun,
    fun.args = fun.args,
    wd_cutfun = wd_cutfun,
    ws_cutfun = ws_cutfun,
    nmin = nmin
  )

  data_summarized <- tidyr::complete(data_summarized, !!wd, !!ws, !!!groupings,
    fill = list(n = 0, freq = rlang::na_dbl)
  )

  if (!is.na(calm)) {
    data_calm <- dplyr::filter(data_summarized, !!ws == "calm")
    freq_calm <- sum(data_calm$freq, na.rm = TRUE)
    freq_calm <- scales::label_percent(accuracy = calm.accuracy, prefix = calm.prefix)(freq_calm)
    data_summarized <- dplyr::filter(data_summarized, !!ws != "calm")
    data_summarized <- dplyr::mutate(data_summarized,
      !!ws := forcats::fct_drop(!!ws)
    )

    calm_layer <- annotate("text", x = 0.5, y = -Inf, label = (freq_calm),
      size = calm.size, color = calm.color
    )

  }

  wd_levels <- levels(dplyr::pull(data_summarized, !!wd))
  wd_breaks <- wd_levels[c(0, 90, 180, 270)/360*length(wd_levels)+1]

  ws_levels <- levels(dplyr::pull(data_summarized, !!ws))
  ws_labels <- ws_levels
  ws_breaks <- seq(1, length(ws_levels))

  data_summarized <- dplyr::mutate(data_summarized,
    ws_num = 1
  )

  # use a real continuous y scale
  # if (!is.na(calm)) {
  #   ws_breaks <- c(calm, seq(1, length(ws_levels)) * ws_binwidth)
  #   data_summarized <- dplyr::mutate(data_summarized,
  #       ws_num = dplyr::if_else(as.numeric(ws) == 1, 1 - calm / ws_binwidth, ws_num)
  #   )
  #
  #
  # } else {
  #   ws_breaks <- seq(0, length(ws_levels)) * ws_binwidth
  # }
  #
  # if (is.na(ws_max)) {
  #   ws_labels <- ws_breaks
  # } else {
  #   ws_breaks <- head(ws_breaks, -1)
  #   ws_labels <- squished_labels(ws_breaks)
  # }

  xexpand <- expansion(add = 0.5)
  yexpand <- expansion(mult = c(yexpand, 0))

  bar_args <- modify_list(list(color = "white", width = 1, size = 0.25), rlang::dots_list(...))
  bar_layer <- rlang::exec(geom_bar, stat = "identity", !!!bar_args)


  plot <- ggplot(data_summarized, aes(x = !!wd, y = .data$ws_num, fill = !!fill)) +
    coord_polar2(start = -2 * pi / 360 * wd_binwidth / 2) +
    bar_layer +
    scale_x_discrete(breaks = wd_breaks, labels = c("N", "O", "S", "W"), expand = xexpand, drop = FALSE) +
    #scale_y_discrete(expand = yexpand) +
    scale_y_continuous(
      expand = yexpand,
      breaks = ws_breaks - 0.5,
      labels = ws_labels
    ) +
    fill_scale +
    theme_rop_windrose() +
    theme(
      axis.title.y = element_text(angle=0)
    ) + labs(
      y = rlang::quo_text(ws),
      fill = rlang::quo_text(fill)
    )

  if (!is.na(calm)) {
    plot <- plot + calm_layer
  }


  return(plot)
}
