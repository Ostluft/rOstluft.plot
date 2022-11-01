#' ggplot wrapper to create a windrose (polar wind-bin frequency plot)
#'
#'
#' @param data tibble containing wind speed, wind direction and/or air pollutant concentration data
#' @param ws symbol giving the wind velocity column name (wind velocity preferably in m/s)
#' @param wd symbol giving the wind direction column name  in degrees
#' @param wd_binwidth numeric, binwidth for wind direction in °, wd_binwidth should fullfill:
#'   `(360 / wd_binwidth) %in% c(4, 8, 16, 32)`
#' @param ws_binwidth numeric, binwidth for wind speed
#' @param ws_max numeric, can be NA, wind speed is squished at this value
#' @param groupings additional groupings. Use helper [grp()] to create. **Necessary** for some facets!
#' @param nmin numeric, minimum number of values for fun, if n < nmin: NA is returned
#' @param fill_scale ggplot2 discrete fill scale, e.g. [ggplot2::scale_fill_gradientn()]
#' @param reverse TRUE/FALSE, should wind speed bin factors be sorted descending (TRUE)
#'   or ascending (FALSE). Usually for wind roses a descending order (higher wind speed on
#'   the outside) is used.
#' @param bg raster map, e.g. ggmap object as plot background
#' @param ... Other arguments passed on to [ggplot2::geom_bar()]. Used
#'   to set an aesthetic to a fixed value. Defaults are `color = "white", width = 1, size = 0.25`
#'
#' @return [ggplot2::ggplot()] object
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_2010-2014.csv")
#'
#' data <-
#'   rOstluft::read_airmo_csv(fn) %>%
#'   rOstluft::rolf_to_openair() %>%
#'   openair::cutData(date, type = "daylight")
#'
#' ggwindrose(data, ws, wd)
#'
#' # squish ws
#' ggwindrose(data, ws, wd, ws_max = 5)
#'
#' # change binning
#' ggwindrose(data, ws, wd, wd_binwidth = 22.5, ws_binwidth = 1.5, ws_max = 4.5)
#'
#' # don't like bar outlines?
#' ggwindrose(data, "ws", "wd", color = "black", ws_max = 4)
#'
#' # bigger outlines
#' ggwindrose(data, ws, wd, ws_max = 5, size = 1)
#'
#' # a map as background
#' bb <- bbox_lv95(2683141, 1249040, 500)
#' bg <- get_stamen_map(bb)
#' ggwindrose(data, ws, wd, ws_max = 5, alpha = 0.8, bg = bg) +
#'   theme(
#'     panel.grid.major = element_line(linetype = 2, color = "black", size = 0.5)
#'    )
#'
#' # another fill scale
#' ggwindrose(data, ws, wd, ws_max = 5,
#'            fill_scale = scale_fill_manual(values = matlab::jet.colors(6)))
#'
#' # reverse the order of ws, but keep the coloring and legend order
#' ggwindrose(data, ws, wd, ws_max = 4, reverse = FALSE,
#'            fill_scale = scale_fill_viridis_d(direction = -1))
#'
#' # faceting: important the faceting variable, must also be in grouping!
#' ggwindrose(data, ws, wd, ws_max = 5, groupings = grp(daylight)) +
#'   facet_wrap(vars(daylight))
#'
#' # you can use groupings to directly mutate the data for faceting.
#' # in this example we define the groupings external for better
#' # readability
#' groupings = grp(
#'   season = cut_season(date, labels = c(DJF = "winter", MAM = "spring",
#'                       JJA = "summer", SON = "autumn")),
#'   year = cut_seasonyear(date, label = "year")
#' )
#'
#' # only three years for smaller plot size and cut the last december
#' # theming remove the NOSW labels and reduce the y spacing between plots
#' data <- dplyr::filter(data, date < lubridate::ymd(20121201))
#' ggwindrose(data, ws, wd, ws_max = 3, groupings = groupings) +
#'   facet_grid(rows = vars(year), cols = vars(season)) +
#'   theme(
#'     axis.text.x = element_blank(),
#'     panel.spacing.y = unit(0, "pt")
#'   )
ggwindrose <- function(data, ws, wd,
                       wd_binwidth = 45,
                       ws_binwidth = 1,
                       ws_max = NA,
                       groupings = grp(),
                       nmin = 1,
                       fill_scale = scale_fill_viridis_d(),
                       reverse = TRUE,
                       bg = NULL,
                       ...

) {

  ws <- rlang::ensym(ws)  # or enquo but summary_wind accept only strings or symbols
  wd <- rlang::ensym(wd)
  wd_cutfun <- cut_wd.fun(binwidth = wd_binwidth)
  ws_cutfun <- cut_ws.fun(binwidth = ws_binwidth, ws_max = ws_max)

  data_summarized <- summary_wind(data, !!ws, !!wd, !!ws, groupings = groupings,
                                  wd_cutfun = wd_cutfun, ws_cutfun = ws_cutfun, nmin = nmin)

  bar_args <- modify_list(list(color = "white", width = 1, size = 0.25), rlang::dots_list(...))
  bar_layer <- rlang::exec(geom_bar, stat = "identity", !!!bar_args)

  # we will convert the wd factor to numeric. so we can always place breaks on NESW

  wd_levels <- levels(dplyr::pull(data_summarized, !!wd))
  wd_breaks <- wd_levels[c(0, 90, 180, 270)/360*length(wd_levels)+1]

  xexpand <- expansion(add = 0.5)

  plot <- ggplot(data_summarized, aes(x = !!wd, y = .data$freq, fill = !!ws)) +
    bar_layer +
    coord_polar2(start = -2 * pi / 360 * wd_binwidth / 2, bg = bg) +
    scale_x_discrete(breaks = wd_breaks, labels = c("N", "O", "S", "W"), expand = xexpand, drop = FALSE) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(), labels = scales::percent) +
    fill_scale +
    guides(fill = guide_legend(title = rlang::quo_text(ws), reverse = !reverse)) +
    theme_rop_windrose()

  return(plot)
}


#' ggplot wrapper to create a windrose with calm support (polar wind-bin frequency plot)
#'
#' @inheritParams ggwindrose
#' @param calm if specified as numeric value all windspeed below this value will be treated
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
#'
#' @return [ggplot2::ggplot()] object
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_2010-2014.csv")
#'
#' data <-
#'   rOstluft::read_airmo_csv(fn) %>%
#'   rOstluft::rolf_to_openair()
#'
#' ggwindrose2(data, ws, wd, calm = 0.3)
#'
#' # squish ws
#' ggwindrose2(data, ws, wd, ws_max = 5, calm = 0.3)
#'
#' #' # a map as background
#' bb <- bbox_lv95(2683141, 1249040, 500)
#' bg <- get_stamen_map(bb)
#' ggwindrose2(data, ws, wd, ws_max = 5, calm = 0.5, alpha = 0.8, bg = bg) +
#'   theme(
#'     panel.grid.major = element_line(linetype = 2, color = "black", size = 0.5)
#'    )
ggwindrose2 <- function(data, ws, wd,
                       wd_binwidth = 45,
                       ws_binwidth = 1,
                       ws_max = NA,
                       groupings = grp(),
                       nmin = 1,
                       fill_scale = scale_fill_viridis_d(),
                       reverse = TRUE,
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
  wd_cutfun <- cut_wd.fun(binwidth = wd_binwidth)
  ws_cutfun <- cut_ws.fun(binwidth = ws_binwidth, ws_max = ws_max, reverse = reverse, calm = calm)

  data_summarized <- summary_wind(data, !!ws, !!wd, !!ws, groupings = groupings,
                                  wd_cutfun = wd_cutfun, ws_cutfun = ws_cutfun, nmin = nmin)


  if (!is.na(calm)) {
    data_calm <- dplyr::filter(data_summarized, !!ws == "calm")
    freq_calm <- sum(data_calm$freq)
    freq_calm <- scales::label_percent(accuracy = calm.accuracy, prefix = calm.prefix)(freq_calm)
    data_summarized <- dplyr::filter(data_summarized, !!ws != "calm")
    data_summarized <- dplyr::mutate(data_summarized,
      !!ws := forcats::fct_drop(!!ws)
    )

    calm_layer <- annotate("text", x = 0.5, y = -Inf, label = (freq_calm),
      size = calm.size, color = calm.color
    )

  }


  bar_args <- modify_list(list(color = "white", width = 1, size = 0.25), rlang::dots_list(...))
  bar_layer <- rlang::exec(geom_bar, stat = "identity", !!!bar_args)

  # we will convert the wd factor to numeric. so we can always place breaks on NESW

  wd_levels <- levels(dplyr::pull(data_summarized, !!wd))
  wd_breaks <- wd_levels[c(0, 90, 180, 270)/360*length(wd_levels)+1]

  xexpand <- expansion(add = 0.5)
  yexpand <- expansion(mult = c(yexpand, 0))

  plot <- ggplot(data_summarized, aes(x = !!wd, y = .data$freq, fill = !!ws)) +
    coord_polar2(start = -2 * pi / 360 * wd_binwidth / 2, bg = bg) +
    bar_layer +
    scale_x_discrete(breaks = wd_breaks, labels = c("N", "O", "S", "W"), expand = xexpand, drop = FALSE) +
    scale_y_continuous(limits = c(0, NA), expand = yexpand, labels = scales::percent) +
    fill_scale +
    guides(fill = guide_legend(title = rlang::quo_text(ws), reverse = !reverse)) +
    theme_rop_windrose()

  if (!is.na(calm)) {
    plot <- plot + calm_layer
  }


  return(plot)
}

