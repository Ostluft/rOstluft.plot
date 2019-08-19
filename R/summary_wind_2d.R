#' Summarise y values over binned wind data, split into u and v components.
#'
#' Input data should be original unbinned data including wind direction and wind velocity;
#' binning is done 2-dimensional over cartesian u and v wind vectors
#'
#' @param data a data.frame or tibble containing the data (wide format).
#' requires input data including at least three columns carrying information regarding:
#' * wind direction (in °)
#' * wind velocity
#' * z-values (e.g. air pollutant concentration)
#' @param ws string giving the wind velocity parameter name (wind velocity preferably in m/s)
#' @param wd string giving the wind direction parameter name  in degrees
#' @param z string giving the parameter name to be summarised
#' @param groups can be NULL, c("u", "v"), "wd_class", "ws_class", ...
#' @param fun function or list of functions for summary.
#' @param fun.args a list of extra arguments to pass to fun.
#' @param nmin numeric, minimum number of values for fun, if n < nmin: NA is returned
#' @param ws_max numeric or NA, maximum wind velocity for binning: above ws_max, z is set NA
#' @param bins numeric, number of bins over the range of values if `!groups %in% c("u", "v")`
#' @param smooth TRUE/FALSE, applies if groups = c("u", "v"); should smoothing of summary results should be performed
#' using [fit_gam_surface()]?
#' @param k numeric, applies if smooth = TRUE; degree of smoothing in smooth term in [fit_gam_surface()]
#' @param extrapolate TRUE/FALSE, applies if smooth = TRUE; [fit_gam_surface()] returns extrapolated (predicted) values for u, v coordinates that otherwise would have have NA for summarised z
#' if extrapolate = TRUE, those values are returned (to a certain degree depending on the value of dist)
#' @param dist numeric, fraction of 1, applies if smooth = TRUE and extrapolate = TRUE; maximum distance to next coordinate-pair at which the result of
#' fit_gam_surface(z) should be returned
#'
#' @return a tibble with summarised data along u and v wind vectors
#'
#' @section Computed variables:
#'
#' * a tibble is returned, binned over u and v, with variables:
#' - wd: wind direction corresponding to midpoint value of u and v
#' - ws: wind velocity corresponding to midpoint value of u and v
#' - u: bins over u (from input wd and ws)
#' - v: bins over v (from input wd and ws)
#' - z: result from fun(z, ...)
#'
#' @export
summary_wind_2d <- function(data, ws, wd, z, groups = NULL, fun = "mean", fun.args = list(), nmin = 3, ws_max = NA, bins = 100,
                             smooth = TRUE, k = 100, extrapolate = TRUE, dist = 0.1) {

  if (is.null(groups) | !("u" %in% groups & "v" %in% groups)) groups <- c(groups, c("u", "v"))
  fun <- c(as.list(fun), "n" = function(x, ...) {sum(!is.na(x))})
  names <- purrr::map2(fun, rlang::names2(fun), function(element, name) {if (name != "") name else element})
  fun <- rlang::set_names(fun, names)
  data <-
    data %>%
    dplyr::mutate(
      u = !!rlang::sym(ws) * sin(pi * !!rlang::sym(wd) / 180),
      v = !!rlang::sym(ws) * cos(pi * !!rlang::sym(wd) / 180)
    )
  uv_max <- pmin(max(abs(c(data$u, data$v)), na.rm = TRUE), ws_max, na.rm = TRUE)
  uv_cuts <- seq(-uv_max, uv_max, length.out = round(sqrt(bins), 0))
  uv_factor <- cut(uv_cuts, breaks = uv_cuts, dig.lab = 10, include.lowest = TRUE)
  uv_mids <- midpoints(uv_factor)
  data <-
    data %>%
    dplyr::mutate(
      u = cut(u, breaks = uv_cuts, dig.lab = 10, include.lowest = TRUE),
      v = cut(v, breaks = uv_cuts, dig.lab = 10, include.lowest = TRUE)
    ) %>%
    stats::na.omit() %>%
    dplyr::group_by_at(groups) %>%
    dplyr::summarise_at(
      .vars = z,
      .funs = fun,
      !!!fun.args
    ) %>%
    dplyr::ungroup() %>%
    tidyr::gather(stat, !!z, -!!groups, -n) %>%
    dplyr::mutate(
      u = as.numeric(u),
      v = as.numeric(v),
      stat = factor(stat),
      freq = n / sum(n, na.rm = TRUE)
    ) %>%
    dplyr::filter(n >= nmin)
  data <-
    expand.grid(
      list(
        u = as.numeric(factor(uv_mids)),
        v = as.numeric(factor(uv_mids)),
        stat = unique((data$stat))
      )) %>%
    stats::na.omit() %>%
    dplyr::tbl_df() %>%
    dplyr::left_join(data, by = c("u", "v", "stat"))
  if (smooth) {
    data <-
      data %>%
      dplyr::group_by(stat) %>%
      dplyr::do({
        fit_gam_surface(., x = "u", y = "v", z = z, weights = pmin(3, .$n) / 3,
                        k = k, extrapolate = extrapolate, dist = dist)
      }) %>%
      dplyr::ungroup()
  }
  data <-
    data %>%
    dplyr::mutate(
      u = dplyr::recode(u, !!!rlang::set_names(uv_mids, as.numeric(uv_factor))),
      v = dplyr::recode(v, !!!rlang::set_names(uv_mids, as.numeric(uv_factor))),
      !!wd := uv2wd(u, v),
      !!ws := sqrt(u^2 + v^2),
      !!z := ifelse(!!rlang::sym(ws) > pmin(Inf, ws_max, na.rm = TRUE), NA, !!rlang::sym(z))
    )

  return(data)
}

