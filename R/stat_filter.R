#' @rdname rOstluft-ggproto
#' @export
StatFilter <- ggproto("StatFilter", Stat,
  required_aes = c("filter"),

  compute_group = function(data, scales, ...)  {
    dplyr::filter(data, .data$filter)
  }
)

#' Filtering data
#'
#' @description
#' Only keeps values where the aesthetic filter evaluates to `TRUE`.
#' Useful for showing only a subset of the data or to highlight some values, without filtering the data outside.
#'
#' Inspired by [metR::stat_subset()].
#'
#' @section Aesthetics:
#'
#' - **filter**: expression evalueted against data
#'
#' @inheritParams ggplot2::stat_identity
#' @param na.rm remove na values befor applying filter
#'
#' @return ggplot2 layer
#' @export
#' @examples
#' require(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' p + stat_filter(aes(filter = mpg > 30), shape = 21, size = 4, stroke = 2, color = "red", fill = NA)
#'
#' p + geom_text(aes(filter = mpg > 32, label = rownames(mtcars)),
#'                 stat = "filter", nudge_y = 0.75)
#'
#' data <- tidyr::crossing(y = 1:dim(volcano)[2], x = 1:dim(volcano)[1])
#' data$value <- array(volcano)
#' ggplot(data, aes(x = x, y = y)) +
#'   geom_contour(aes(z = value)) +
#'   stat_filter(aes(filter = value >= 150 & value <= 160), shape = 3, color = "red")
stat_filter <- function(mapping = NULL, data = NULL, geom = "point", position = "identity",
                        show.legend = FALSE, inherit.aes = TRUE, na.rm = TRUE, ...) {
  layer(
    stat = StatFilter, data = data, mapping = mapping, geom = geom, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


