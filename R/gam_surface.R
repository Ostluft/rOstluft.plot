#' Fits a smooth surface to u, v, y data using generalised additive models
#' 
#' @description This is based on mgcv::gam()
#' 
#' @param u numeric vector with u (wind) component or x coordinate, respectively
#' @param v numeric vector with v (wind) component or y coordinate, respectively
#' @param z numeric vector with u component or x coordinate, respectively
#' @param weights optional: vector of weights for fitting u, v value pair
#' @param k numeric, smoothing degree in gam model: mgcv::gam(z ~ s(u, v, k = k)
#' @param extrapolate TRUE/FALSE, result of fit extends over NA values in z, thus providing a way of extrapolation. If FALSE,
#' only u, v pairs with !is.na(z) are returned, if TRUE, also fitted z values within a certain distance (dist) from u, v are returned 
#' based on mgcv::exclude.too.far()
#' @param force_positive TRUE/FALSE, shall fitted values forced to be positive?
#' @param dist input for mgcv::exclude.too.far(): how far away counts as too far. Grid and data are first scaled so that the grid lies 
#' exactly in the unit square, and dist is a distance within this unit square
#' 
#' @return tibble with variables u, v, z
#' 
gam_surface <- function(u, v, z, weights = NULL, k = 100, extrapolate = FALSE, force_positive = TRUE, dist = 0.05) {
  if (force_positive) n <- 0.5 else n <- 1
  data <- tibble::tibble(
    u = u,
    v = v,
    z = z
  )
  data$z <- data$z^n
  data$id <- 1:nrow(data)
  index <- which(!is.na(data$z))
  m <- mgcv::gam(z ~ s(u, v, k = k, bs = "gp"), 
                 data = data,
                 weights = weights,
                 method = 'REML', 
                 control =  mgcv::gam.control(nthreads = parallel::detectCores() - 1),
                 family = gaussian())
  if (extrapolate) {
    pred <- predict(m, newdata = data, type = 'response')^(1/n)
  } else {
    pred <- predict(m, type = 'response')^(1/n)
  }
  pred <- 
    tibble::tibble(
      id = as.numeric(names(pred)),
      z = pred
    )
  data <-
    data %>% 
    dplyr::select(-z) %>% 
    dplyr::left_join(pred, by = "id") %>% 
    dplyr::mutate(
      z = ifelse(mgcv::exclude.too.far(.$u, .$v, data$u[index], data$v[index], dist = dist), NA, z)
    ) %>% 
    dplyr::select(-id)
  
  return(data)
}
