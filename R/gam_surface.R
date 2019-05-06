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
