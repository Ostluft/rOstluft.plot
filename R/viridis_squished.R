#' squish outliers or extreme values in scales
#'
#' The scale only applies the limits to the determination of the color.
#' The underlying data isn't squish. All values outside the limit are
#' simple colored the same.
#'
#' @param limits for the scale
#' @param breaks A numeric vector of positions for breaks
#' @param labels One of:
#'   - `NULL` for no labels
#'   - `waiver()` for the default labels computed by the
#'     transformation object. Adds leading ">" for last element
#'   - A character vector giving labels (must be same length as `breaks`)
#'   - A function that takes the breaks as input and returns labels
#'     as output. Adds leading ">" for last element
#' @param ... Other arguments passed on to `ggplot2::scale_colour_viridis_c()` or `scale_fill_viridis_c()`
#'
#' @return scale
#' @export
#' @rdname scale_viridis_squished
#'
#' @examples
#' library(ggplot2)
#'
#' fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_2010-2014.csv")
#'
#' df <-
#'   rOstluft::read_airmo_csv(fn) %>%
#'   dplyr::filter(starttime < lubridate::ymd(20140101)) %>%
#'   rOstluft::rolf_to_openair()
#'
#' # data with outliers / extreme values
#' ggyearday(df, time = "date", z = "PM10")
#'
#' # simple squished scale
#' fill_scale <- scale_fill_viridis_squished(
#'   limits = c(0, 75),
#'   breaks = c(0, 25, 50, 75),
#'   direction = -1,
#'   na.value = NA,
#'   option = "A"
#' )
#' ggyearday(df, time = "date", z = "PM10", fill_scale = fill_scale) +
#'   facet_wrap(year~., scales = "free_x", ncol = 1)
#'
#' # add unit to scale (or any other label formating function)
#' fill_scale <- scale_fill_viridis_squished(
#'   limits = c(0, 75),
#'   breaks = c(0, 25, 50, 75),
#'   labels = scales::unit_format(unit = "\u00b5g/m3"),
#'   direction = -1,
#'   na.value = NA,
#'   option = "A"
#' )
#' ggyearday(df, time = "date", z = "PM10", fill_scale = fill_scale)
#'
#' # custom labels for breaks
#' fill_scale <- scale_fill_viridis_squished(
#'   limits = c(0, 75),
#'   breaks = c(0, 25, 50, 75),
#'   labels = c("A", "B", "C", "D"),
#'   direction = -1,
#'   na.value = NA,
#'   option = "A"
#' )
#' ggyearday(df, time = "date", z = "PM10", fill_scale = fill_scale)
#'
#' # No labels for breaks
#' fill_scale <- scale_fill_viridis_squished(
#'   limits = c(0, 75),
#'   breaks = c(0, 25, 50, 75),
#'   labels = NULL,
#'   direction = -1,
#'   na.value = NA,
#'   option = "A"
#' )
#' ggyearday(df, time = "date", z = "PM10", fill_scale = fill_scale)
#'
#' # simple squished jet gradientn scale
#' fill_scale <- scale_fill_gradientn_squished(
#'   limits = c(0, 75),
#'   breaks = c(0, 25, 50, 75),
#'   colors = matlab::jet.colors(20),
#'   na.value = NA
#' )
#' ggyearday(df, time = "date", z = "PM10", fill_scale = fill_scale)
scale_fill_viridis_squished <- function(limits, breaks, labels = waiver(), ...) {
  if(missing(limits) & rlang::is_vector(limits, 2)) stop("limits with 2 elements is required")
  if(missing(breaks) & rlang::is_vector(breaks)) stop("breaks as vector is required")

  labs <- squished_labels(labels)

  scale_fill_viridis_c(limits = limits, breaks = breaks, labels = labs, oob = scales::squish, ...)
}


#' @export
#' @rdname scale_viridis_squished
scale_color_viridis_squished <- function(limits, breaks, labels = waiver(), ...) {
  if(missing(limits) & rlang::is_vector(limits, 2)) stop("limits with 2 elements is required")
  if(missing(breaks) & rlang::is_vector(breaks)) stop("breaks as vector is required")

  labs <- squished_labels(labels)

  scale_color_viridis_c(limits = limits, breaks = breaks, labels = labs, oob = scales::squish, ...)
}


#' @export
#' @rdname scale_viridis_squished
scale_fill_gradientn_squished <- function(limits, breaks, labels = waiver(), ...) {
  if(missing(limits) & rlang::is_vector(limits, 2)) stop("limits with 2 elements is required")
  if(missing(breaks) & rlang::is_vector(breaks)) stop("breaks as vector is required")

  labs <- squished_labels(labels)

  scale_fill_gradientn(limits = limits, breaks = breaks, labels = labs, oob = scales::squish, ...)
}


#' @export
#' @rdname scale_viridis_squished
scale_color_gradientn_squished <- function(limits, breaks, labels = waiver(), ...) {
  if(missing(limits) & rlang::is_vector(limits, 2)) stop("limits with 2 elements is required")
  if(missing(breaks) & rlang::is_vector(breaks)) stop("breaks as vector is required")

  labs <- squished_labels(labels)

  scale_color_gradientn(limits = limits, breaks = breaks, labels = labs, oob = scales::squish, ...)
}


#' generates labels for squished scales
#'
#' @param labels One of:
#'   - `NULL` for no labels
#'   - `waiver()` for the default labels computed by the
#'     transformation object
#'   - A character vector giving labels (must be same length as `breaks`)
#'   - A function that takes the breaks as input and returns labels
#'     as output
#'  Additional if not NULL the last element will be formated with a leading ">"
#'
#' @return function, labels for scales
#' @keywords internal
#' @noRd
squished_labels <- function(labels) {
  if (is.null(labels)) {
    labs <- NULL
  } else if (is.waive(labels)) {
    labs <- function(x) {
      purrr::map_at(x, length(x), format_sprintf(">%s"))
    }
  } else if (is.function(labels)) {
    labs <- function(x) {
      x <- labels(x)
      x <- purrr::map_at(x, length(x), format_sprintf(">%s"))
    }
  } else {
    labs <- labels
  }
  labs
}





