#' Fits a smooth surface to x, y, z data using generalised additive models
#'
#' @description This is based on [mgcv::gam()] and derived from [openair::polarPlot()]
#'
#' @param data a data.frame or tibble containing the data (wide format)
#' @param x string giving the u (wind) component or x coordinate, respectively
#' @param y string giving the v (wind) component or y coordinate, respectively
#' @param z string giving the response variable
#' @param weights vector of weights for fitting x, y value pair; can be NULL
#' @param k numeric, smoothing degree in gam model mgcv::gam(z ~ s(x, y, k = k)
#' @param extrapolate TRUE/FALSE, result of fit extends over NA values in z, thus providing a way of extrapolation. If FALSE,
#' only u, v pairs with !is.na(z) are returned, if TRUE, also fitted z values within a certain distance (dist) from x, y are returned
#' based on mgcv::exclude.too.far()
#' @param force_positive TRUE/FALSE, shall fitted values forced to be positive?
#' @param dist input for [mgcv::exclude.too.far()]: how far away counts as too far. Grid and data are first scaled so that the grid lies
#' exactly in the unit square, and dist is a distance within this unit square
#'
#' @return tibble with variables u, v, z
#' @export
fit_gam_surface <- function(data, x, y, z, weights = NULL, k = 100, extrapolate = FALSE, force_positive = TRUE, dist = 0.05) {
  if (force_positive) force_positive <- 0.5 else force_positive <- 1
  data <-
    data %>%
    dplyr::mutate(
      !!z := (!!rlang::sym(z))^force_positive,
      id = 1:nrow(data)
    )
  index <- which(!is.na(dplyr::pull(data, !!rlang::sym(z))))
  frml <- stats::as.formula(paste0(z," ~ s(",x,", ",y,", k = ",k,")"))
  m <- mgcv::bam(frml,
                 data = data,
                 weights = weights,
                 method =  "GCV.Cp", # "fREML"
                 control = mgcv::gam.control(nthreads = parallel::detectCores() - 1),
                 family = stats::gaussian())
  if (extrapolate) {
    pred <- mgcv::predict.gam(m, newdata = data, type = 'response')^(1/force_positive)
  } else {
    pred <- mgcv::predict.gam(m, type = 'response')^(1/force_positive)
  }
  pred <-
    tibble::tibble(
      id = as.numeric(names(pred)),
      !!z := pred
    )
  data <-
    data %>%
    dplyr::select(-!!rlang::sym(z)) %>%
    dplyr::left_join(pred, by = "id")
  if (extrapolate) {
    measx <- dplyr::pull(data, x)[index]
    measy <- dplyr::pull(data, y)[index]
    data <-
      data %>%
      dplyr::mutate(
        !!z := ifelse(mgcv::exclude.too.far(!!rlang::sym(x), !!rlang::sym(y),  measx,  measy, dist = dist), NA, !!rlang::sym(z))
      )
  }

  return(dplyr::select(data, -"id"))
}
