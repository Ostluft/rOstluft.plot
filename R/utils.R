# ... ich schmeisse Kleinkram vorerst hier rein, räume am Schluss dann aber wieder auf!




#' Wrapper to cut values into classes
#' 
#' using ggplot2::cut_interval(), ggplot2::cut_width(), or base::curt()
#'
#' @param y object for cutting to be applied
#' @param y_cuts named list with one of the following items: list(nclass = ..., cutwidth = ...);
#' in case nclass is provided, cut_interval(y, n = y_cuts$nclass) is used for cutting, if
#' cut_width is provided, cut_width(y, width = y_cuts$cutwidth) is used
#' @param y_boundary numeric; lower boundary used for cutting
#' @param dig.lab integer; number of digits for labelling the classes derived from cutting
#'
#' @return factor, of same length as y; classes according to the used cut function
cut_fun <- function(y, y_cuts, ...) { # in helpers verschieben
  if (!is.null(y_cuts[[1]])) {
    y <- 
      switch(names(y_cuts)[1],
             nclass = ggplot2::cut_interval(y, n = y_cuts$nclass, dig.lab = y_cuts$dig_lab, ...),
             cutwidth = ggplot2::cut_width(y, width = y_cuts$cutwidth, boundary = y_cuts$y_boundary, ...),
             breaks = base::cut(y, breaks = y_cuts$breaks, dig.lab = y_cuts$dig_lab,...)
      )
  }
  return(y)
}



wd_classes <- function(wd, wd_cutwidth = 45, ...) { # in helpers verschieben
  if ((360 / wd_cutwidth) %in% c(4, 8, 12, 16)) {
    wd <- (wd + wd_cutwidth / 2) %% 360
  }
  wd <- ggplot2::cut_width(wd, width = wd_cutwidth, closed = "left", boundary = 0, ...)
  return(wd)
}



y_classes <- function(y, y_cuts = list(nclass = 4, y_boundary = 0, y_cap = Inf, dig_lab = 1), ...) {
  y <- cut_fun(pmin(y, y_cuts$y_cap, na.rm = TRUE), y_cuts = y_cuts, ...) 
  return(y)
}


recode_last_class_label <- function(factor_var) {
  maxlevel <- tail(unlist(strsplit(levels(factor_var), split = ",", fixed = TRUE)), 2)[1]
  levels(factor_var)[length(levels(factor_var))] <- paste0(">", substring(maxlevel, 2, nchar(maxlevel)))
  return(factor_var)
}



midpoints <- function(x){
  lower <- as.numeric(gsub(",.*","",gsub("\\(|\\[|\\)|\\]","", x)))
  upper <- as.numeric(gsub(".*,","",gsub("\\(|\\[|\\)|\\]","", x)))
  return(lower + (upper - lower) / 2)
}



uv2wd <- function(u, v) { #' von hier: https://github.com/environmentalinformatics-marburg/Rsenal/blob/master/R/uv2wdws.R
  degrees <- function(radians) 180 * radians / pi
  mathdegs <- degrees(atan2(v, u))
  wdcalc <- ifelse(mathdegs > 0, mathdegs, mathdegs + 360)
  wd <- ifelse(wdcalc < 270, 270 - wdcalc, 270 - wdcalc + 360)
  return(wd)
}



bbox <- function(bb) {
  c("left" = bb$lon[bb$type == "lb"], "bottom" = bb$lat[bb$type == "lb"], "right" = bb$lon[bb$type == "rt"], "top" = bb$lat[bb$type == "rt"])
}





