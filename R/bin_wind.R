#' Summarise y values over binned wind data.
#'
#' Input data should be original unbinned data.
#' 1-dimensional binning and calculating summary statistics over wind direction and/or wind velocity bins, respectively.
#'
#' @param data a data.frame or tibble containing the data (wide format)
#' @param ws string giving the wind velocity parameter name (wind velocity preferably in m/s)
#' @param wd string giving the wind direction parameter name  in degrees
#' @param z string giving the parameter name to be summarised
#' @param groups character string, can be NULL, 'wd', 'ws', ...
#' @param fun function or list of functions for summary.
#' @param fun.args a list of extra arguments passed on to fun.
#' @param nmin numeric, minimum number of values for fun, if n < nmin: NA is returned
#' @param wd_cutfun function, cut function for wind direction (to create bins)
#' @param wd_offset numeric, offset for wind_direction (in °) if groups == "wd"; bins are then calculated
#'   over `(wd + wd_offset) %% 360`
#' @param ws_cutfun function, cut function for wind speed
#'
#' @return a tibble with summarised data
#'
#' @section Computed variables:
#'
#' * If groups = NULL: groups = "wd". In this case, bins are calculated over wind direction;
#'   a tibble including wd and summarised z is returned
#' * groups can be strings for other variables in data; then fun is applied over those;
#'   a tibble including groups and summarised z is returned
#'
#' @export
stat_bin_wind <- function(data, ws, wd, z, groups = NULL, fun = "mean", fun.args = list(), nmin = 3,
                          wd_cutfun = cut_wd.fun(binwidth = 45), wd_offset = 0,
                          ws_cutfun = cut_ws.fun(binwidth = 1, ws_max = NA)) {

  if (is.null(groups)) groups <- wd
  fun <- c(as.list(fun), "n" = function(x, ...) {sum(!is.na(x))})
  names <- purrr::map2(fun, rlang::names2(fun), function(element, name) {if (name != "") name else element})
  fun <- rlang::set_names(fun, names)
  data <-
    data %>%
    dplyr::mutate(
      !!wd := wd_cutfun(!!rlang::sym(wd)),
      !!ws := ws_cutfun(!!rlang::sym(ws))
    ) %>%
    stats::na.omit() %>%
    dplyr::group_by_at(groups) %>%
    dplyr::summarise_at(
      .vars = z,
      .funs = fun,
      !!!fun.args
    ) %>%
    dplyr::ungroup() %>%
    tidyr::gather(key = "stat", value = !!z, -!!groups, -n) %>%
    dplyr::mutate(stat = factor(stat), freq = n / sum(n, na.rm = TRUE)) %>%
    dplyr::filter(n >= nmin)

  return(data)
}

