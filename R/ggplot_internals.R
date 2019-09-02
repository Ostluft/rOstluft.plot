# This module contains interval ggplot2 code, that isn't exported



#' Given a theme object and element name, return a grob for the element
#' @keywords internal
#' @noRd
element_render <- function(theme, element, ..., name = NULL) {

  # Get the element from the theme, calculating inheritance
  el <- calc_element(element, theme)
  if (is.null(el)) {
    message("Theme element ", element, " missing")
    return(zeroGrob())
  }

  grob <- element_grob(el, ...)
  ggname(paste(element, name, sep = "."), grob)
}

#' Rename elements in a list, data.frame or vector
#'
#' This is akin to `dplyr::rename` and `plyr::rename`. It renames elements given
#' as names in the `replace` vector to the values in the `replace` vector
#' without touching elements not referenced.
#'
#' @param x A data.frame or a named vector or list
#' @param replace A named character vector. The names identifies the elements in
#' `x` that should be renamed and the values gives the new names.
#'
#' @return `x`, with new names according to `replace`
#'
#' @keywords internal
#' @noRd
rename <- function(x, replace) {
  current_names <- names(x)
  old_names <- names(replace)
  missing_names <- setdiff(old_names, current_names)
  if (length(missing_names) > 0) {
    replace <- replace[!old_names %in% missing_names]
    old_names <- names(replace)
  }
  names(x)[match(old_names, current_names)] <- as.vector(replace)
  x
}

# Name ggplot grid object
# Convenience function to name grid objects
#
# @keyword internal
#' @noRd
ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}


len0_null <- function(x) {
  if (length(x) == 0)
    NULL
  else
    x
}

# More performant modifyList without recursion
modify_list <- function(old, new) {
  for (i in names(new)) old[[i]] <- new[[i]]
  old
}

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}


#' checks if x is a wavier object
#'
#' @seealso `ggplot2::waiver()`
#'
#' @param x object to check
#'
#' @return TRUE/FALSE
#'
#' @keywords internal
#' @noRd
is.waive <- function(x) {
  inherits(x, "waiver")
}
