
#' recode highest '[x-y)'-style factor level to yield '>y'
#'
#' @export
recode_last_class_label <- function(factor_var) {
  maxlevel <- utils::tail(unlist(strsplit(levels(factor_var), split = ",", fixed = TRUE)), 2)[1]
  levels(factor_var)[length(levels(factor_var))] <- paste0(">", substring(maxlevel, 2, nchar(maxlevel)))
  return(factor_var)
}



#' returns class midpoint-values for '[x-y)'-style factor levels
#'
#' @export
midpoints <- function(x){
  lower <- as.numeric(gsub(",.*","",gsub("\\(|\\[|\\)|\\]","", x)))
  upper <- as.numeric(gsub(".*,","",gsub("\\(|\\[|\\)|\\]","", x)))
  return(lower + (upper - lower) / 2)
}
