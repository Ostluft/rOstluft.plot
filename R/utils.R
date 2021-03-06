#' calculate wind direction from u, v wind components
#'
#' copied from https://github.com/environmentalinformatics-marburg/Rsenal/blob/master/R/uv2wdws.R
#'
#' @param u a vector of u components
#' @param v a vector of v components
#'
#' @return a vector of wind direction in °
#'
#' @export
uv2wd <- function(u, v) {
  degrees <- function(radians) 180 * radians / pi
  mathdegs <- degrees(atan2(v, u))
  wdcalc <- ifelse(mathdegs > 0, mathdegs, mathdegs + 360)
  wd <- ifelse(wdcalc < 270, 270 - wdcalc, 270 - wdcalc + 360)
  return(wd)
}




#' set up a bounding box for ggmap::get_map()
#'
#' Converts a google bounding box to a stamen boundings box
#'
#' @param bb boundings box specifications
#'
#' @export
bbox <- function(bb) {
  c("left" = bb$lon[bb$type == "lb"], "bottom" = bb$lat[bb$type == "lb"], "right" = bb$lon[bb$type == "rt"], "top" = bb$lat[bb$type == "rt"])
}


#' Wrapper for sprintf string formatting
#'
#' Can be used to provide format functions for labels in ggpltos
#'
#' @param fmt sprintf formatting string. see `sprintf()`
#'
#' @return function formatting x with sprintf
#' @export
#'
#' @examples
#' # add one leading zeroes for 1 digits numbers
#' add_leading_zeroes <- format_sprintf("%02d")
#' add_leading_zeroes(1:10)
format_sprintf <- function(fmt) {
  function(x) {
    sprintf(fmt, x)
  }
}


quo_as_symbol <- function(quo) {
  rlang::sym(rlang::quo_get_expr(quo))
}

quo_is_character <- function(quo) {
  rlang::is_character(rlang::quo_get_expr(quo))
}



#' Pass grouping definitions into functions
#'
#' Inspired by [dplyr::vars()], but converting strings to symbols and auto names all arguments
#'
#' @param ... Variables to group by. These arguments are automatically
#'   [quoted][rlang::quo] and later [evaluated][rlang::eval_tidy] in the
#'   context of the data frame. They support [unquoting][rlang::quasiquotation].
#'
#' @return named list containing quosures or symbols
#' @export
#'
#' @examples
#' fn <- rOstluft.data::f("Zch_Stampfenbachstrasse_d1_2017.csv")
#' data <- rOstluft::read_airmo_csv(fn)
#'
#' # adding group_nest to quickly glance over the groups
#' groupby <- function(df, group = grp()) {
#'   dplyr::group_by(df, !!!group) %>%
#'     dplyr::group_nest()
#' }
#'
#' # no grouping -> everything will be nested
#' groupby(data)
#'
#' # use a symbol, string or an expression
#' groupby(data, grp(site, "unit", lubridate::year(starttime)))
#'
#' # autonaming works fine with strings and symbols, but for expressions
#' # it probably a good idea to provide a name:
#' groupby(data, grp(site, year = lubridate::year(starttime)))
grp <- function(...) {
  quos <- as.list(rlang::quos(...))
  quos <- purrr::modify_if(quos, quo_is_character, quo_as_symbol)
  rlang::exprs_auto_name(quos)
}



#' Ensure that all elements of a list of expressions are named
#'
#' Nearly identical to [rlang::exprs_auto_name()], but [rlang::as_name()]
#' is used instead of [rlang::as_label()]. For String items the string will
#' returned without wrapping in double quotes. The naming of functions and
#' formulas is not optimal, it is better to manually name theme.
#'
#' @param exprs  A list of expressions.
#'
#' @return A named list of expressions
#' @export
#'
#' @examples
#' funs <- list(
#'   "mean",
#'   function(x) stats::quantile(x, probs = 0.95),
#'   ~ stats::quantile(., probs = 0.95),
#'   q95 = function(x) stats::quantile(x, probs = 0.95)
#' )
#'
#' auto_name(funs)
#'
#' # exprs_autoname adds double quotes to strings
#' rlang::exprs_auto_name(funs)
auto_name <- function(exprs) {
  have_name <- rlang::have_name(exprs)
  if (any(!have_name)) {
    nms <- purrr::map_chr(exprs[!have_name], rlang::quo_name)
    names(exprs)[!have_name] <- nms
  }
  exprs
}



