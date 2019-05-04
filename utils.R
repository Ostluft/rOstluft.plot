require(ggplot2)
require(dplyr)
require(tidyr)


#' Wrapper to cut values into classes using ggplot2::cut_interval() or ggplot2::cut_width(); for use with ggpolar()
#'
#' @param y object for cutting to be applied
#' @param y_cuts named list with one of the following items: list(nclass = ..., cutwidth = ...);
#' in case nclass is provided, cut_interval(y, n = y_cuts$nclass) is used for cutting, if
#' cut_width is provided, cut_width(y, width = y_cuts$cutwidth) is used
#' @param y_boundary numeric; lower boundary used for cutting
#' @param dig.lab integer; number of digits for labelling the classes derived from cutting
#'
#' @return factor, of same length as y; classes according to the used cut function
#'
#' @keywords internal
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
  stopifnot()
  if ((360 / wd_cutwidth) %in% c(4, 8, 12, 16)) {
    wd <- (wd + wd_cutwidth / 2) %% 360
  }
  wd <- ggplot2::cut_width(wd, width = wd_cutwidth, closed = "left", boundary = 0, ...)
  return(wd)
}



recode_last_class_label <- function(factor_var) {
  maxlevel <- tail(unlist(strsplit(levels(factor_var), split = ",", fixed = TRUE)), 2)[1]
  levels(factor_var)[length(levels(factor_var))] <- paste0(">", substring(maxlevel, 2, nchar(maxlevel)))
  return(factor_var)
}





y_classes <- function(y, y_cuts = list(nclass = 4, y_boundary = 0, y_cap = Inf, dig_lab = 1), ...) {
  y <- cut_fun(pmin(y, y_cuts$y_cap, na.rm = TRUE), y_cuts = y_cuts, ...) 
  return(y)
}



bbox <- function(bb) {
  c("left" = bb$lon[bb$type == "lb"], "bottom" = bb$lat[bb$type == "lb"], "right" = bb$lon[bb$type == "rt"], "top" = bb$lat[bb$type == "rt"])
}






# ngrrhh..

# polar_cartesian <- function(x, y, offset = pi/2) {
#   maxy <- max(y, na.rm = TRUE)
#   radial <- ggforce::radial_trans(r.range = c(pmin(0, min(y, na.rm = TRUE)), max(y, na.rm = TRUE)), a.range = range(x), offset = offset)
#   df <- 
#     radial$transform(y, x) %>% 
#     mutate(
#       x = .data$x * maxy,
#       y = .data$y * maxy
#     )
#   
#   return(df)
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# StatWindCartesian <- ggproto("StatWindCartesian", Stat,
#                              
#                              compute_group = function(data, scales, fun.y = "mean", wd_cutwidth = 45) {
#                                data <- 
#                                  tibble::tibble(
#                                    x = wd_classes(data$x, wd_cutwidth),
#                                    y = data$y
#                                  )
#                                df <- 
#                                  data %>% 
#                                  dplyr::group_by(x) %>% 
#                                  dplyr::summarise_at(
#                                    .vars = "y",
#                                    .funs = fun.y
#                                  ) %>% 
#                                  dplyr::ungroup()
#                                offset <- ifelse(0 %in% data$x, pi/2 - 2 * pi * wd_cutwidth / 2 / 360, pi/2)
#                                df <- 
#                                  polar_cartesian(df$x, df$y, offset = offset)
#                                df
#                              },
#                              
#                              required_aes = c("x", "y")
# )
# 
# 
# 
# 
# stat_wind_cartesian <- function(mapping = NULL, data = NULL, geom = "polygon",
#                                 position = "identity", na.rm = TRUE, show.legend = NA, 
#                                 inherit.aes = TRUE, alpha = 0.5, wd_cutwidth = 45, ...) {
#   ggplot2::layer(
#     stat = StatWindCartesian, data = data, mapping = mapping, geom = geom, 
#     position = position, show.legend = show.legend, inherit.aes = inherit.aes,
#     params = list(na.rm = na.rm, alpha = alpha, wd_cutwidth = wd_cutwidth, ...)
#   )
# }
# 
# 
# 
# arcPaths <- function(data, n) {
#   trans <- radial_trans(c(0, 1), c(0, 2 * pi), pad = 0)
#   data <- data[data$start != data$end, ]
#   data$nControl <- ceiling(n / (2 * pi) * abs(data$end - data$start))
#   data$nControl[data$nControl < 3] <- 3
#   extraData <- !names(data) %in% c('r0', 'r', 'start', 'end')
#   paths <- lapply(seq_len(nrow(data)), function(i) {
#     path <- data.frame(
#       a = seq(data$start[i], data$end[i], length.out = data$nControl[i]),
#       r = data$r[i]
#     )
#     if ('r0' %in% names(data)) {
#       if (data$r0[i] != 0) {
#         path <- rbind(
#           path,
#           data.frame(a = rev(path$a), r = data$r0[i])
#         )
#       } else {
#         path <- rbind(
#           path,
#           data.frame(a = data$start[i], r = 0)
#         )
#       }
#     }
#     path$group <- i
#     path$index <- seq(0, 1, length.out = nrow(path))
#     path <- cbind(path, data[rep(i, nrow(path)), extraData, drop = FALSE])
#   })
#   paths <- do.call(rbind, paths)
#   paths <- cbind(
#     paths[, !names(paths) %in% c('r', 'a')],
#     trans$transform(paths$r, paths$a)
#   )
#   paths$x <- paths$x + paths$x0
#   paths$y <- paths$y + paths$y0
#   if ('explode' %in% names(data)) {
#     exploded <- data$explode != 0
#     if (any(exploded)) {
#       exploder <- trans$transform(
#         data$explode[exploded],
#         data$start[exploded] + (data$end[exploded] - data$start[exploded]) / 2
#       )
#       explodedPaths <- paths$group %in% which(exploded)
#       exploderInd <- as.integer(factor(paths$group[explodedPaths]))
#       paths$x[explodedPaths] <-
#         paths$x[explodedPaths] + exploder$x[exploderInd]
#       paths$y[explodedPaths] <-
#         paths$y[explodedPaths] + exploder$y[exploderInd]
#     }
#   }
#   paths[, !names(paths) %in% c('x0', 'y0', 'exploded')]
# }
# 
# 
# 
# 
# StatWindrose <- ggproto("StatWindrose", Stat,
#                         
#                         compute_panel = function(data, scales, n = 360, wd_cutwidth = 45) {
#                           
#                           data <- 
#                             tibble::tibble(
#                               wd = wd_classes(data$wd, wd_cutwidth),
#                               ws = data$ws
#                             )
#                           df <- 
#                             data %>% 
#                             dplyr::group_by(wd, ws) %>% 
#                             dplyr::summarise(r = n()) %>% 
#                             group_by(wd) %>% 
#                             mutate(
#                               r = cumsum(r) / nrow(data), 
#                               r0 = dplyr::lag(r),
#                               r0 = ifelse(is.na(r0), 0, r0)
#                             ) %>% 
#                             dplyr::ungroup() %>%
#                             mutate(
#                               x0 = 0, 
#                               y0 = 0,
#                               start = (as.numeric(wd) - 1) * wd_cutwidth / 360 * 2 * pi - wd_cutwidth / 360 * 2 * pi / 2 * ifelse(0 %in% data$wd, 1, 0),
#                               end = as.numeric(wd) * wd_cutwidth / 360 * 2 * pi - wd_cutwidth / 360 * 2 * pi / 2 * ifelse(0 %in% data$wd, 1, 0)
#                             ) # %>% dplyr::filter(!is.na(wd) & !is.na(ws) & !is.infinite(wd) & !is.infinite(ws))
#                           str(df)
#                           arcPaths(df, n)
#                         },
#                         
#                         required_aes = c("wd", "ws")
# )
# 
# 
# 
# 
# stat_windrose <- function(mapping = NULL, data = NULL, geom = 'arc_bar',
#                           position = 'identity', n = 360, na.rm = FALSE,
#                           show.legend = NA, inherit.aes = TRUE, wd_cutwidth = 45, ...) {
#   layer(
#     stat = StatWindrose, data = data, mapping = mapping, geom = geom,
#     position = position, show.legend = show.legend, inherit.aes = inherit.aes,
#     params = list(na.rm = na.rm, n = n, wd_cutwidth = wd_cutwidth, ...)
#   )
# }
# 
# 
# 
# 
# 
# 
# 
# calc_cartesian_polar_wd_stats <- function(df, wd, y, group, stat = "mean", wd_cutwidth = 45, y_cuts, ...) {
#   df <- 
#     df %>% 
#     calc_wd_classes(wd, y, group, y_cuts, wd_cutwidth)
#   if ("y_classes" %in% names(df)) {
#     y_classes <- "y_classes"
#   } else {
#     y_classes <- NULL
#   }
#   df <- 
#     df %>% 
#     group_by_at(c(wd, group, y_classes)) %>% 
#     dplyr::summarise_at(
#       .vars = y, 
#       .funs = list(stat),
#       ...
#     ) %>% 
#     ungroup() %>% 
#     mutate(
#       x = as.numeric(!!sym(wd))
#     )
#   df <- polar_cartesian(df," x", y)
#   
#   return(df)
# }
# 
# 
# 
# 
# 
# 
# 
# # https://github.com/tidyverse/ggplot2/blob/master/R/coord-cartesian-.r 
# coord_polar_cartesian <- function(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") { 
#   ggproto(NULL, CoordPolarCartesian, 
#           limits = list(x = xlim, y = ylim), 
#           ratio = ratio, 
#           expand = expand, 
#           clip = clip 
#   ) 
# } 
# 
# 
# # coord_polar_cartesian <- function(plot) {
# #   xrange <- ggplot_build(plot)$layout$panel_scales_x[[1]]$range$range
# #   yrange <- ggplot_build(plot)$layout$panel_scales_y[[1]]$range$range
# #   maxrange <- max(abs(c(xrange, yrange)))
# #   coord <- coord_equal(xlim = c(-maxrange, maxrange), ylim = c(-maxrange, maxrange)) 
# #   return(coord)
# # }
# 
# 
# scale_range <- function(scale, limits = NULL, expand = TRUE) { 
#   expansion <- if (expand) c(0.05,0.05) else c(0, 0)  # if (expand) expand_default(scale) else c(0, 0) 
#   if (is.null(limits)) { 
#     scale$dimension(expansion) 
#   } else { 
#     range <- range(scale$transform(limits)) 
#     scales::expand_range(range, expansion[1], expansion[2]) 
#   } 
# } 
# 
# 
# 
# CoordPolarCartesian <- ggproto("CoordPolarCartesian", CoordCartesian, 
#                                
#                                is_free = function() FALSE, 
#                                
#                                aspect = function(self, ranges) { 
#                                  diff(ranges$y.range) / diff(ranges$x.range) * self$ratio 
#                                }, 
#                                
#                                setup_panel_params = function(self, scale_x, scale_y, params = list()) { 
#                                  train_cartesian <- function(scale, limits, name) { 
#                                    range <- scale_range(scale, limits, self$expand) 
#                                    out <- scale$break_info(range) 
#                                    out$arrange <- scale$axis_order() 
#                                    names(out) <- paste(name, names(out), sep = ".") 
#                                    out 
#                                  } 
#                                  
#                                  x <- train_cartesian(scale_x, self$limits$x, "x") 
#                                  y <- train_cartesian(scale_y, self$limits$y, "y") 
#                                  
#                                  lim <- max(abs(c(x$x.range, y$y.range)), na.rm = TRUE) 
#                                  x[[1]] <- c(-lim, lim) 
#                                  y[[1]] <- c(-lim, lim) 
#                                  x[[2]] <- as.character(abs(as.numeric(x[[2]])))
#                                  y[[2]] <- as.character(abs(as.numeric(x[[2]])))
#                                  
#                                  c(x, y) 
#                                } 
# ) 
# 
# # p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
# # p + coord_fixed(ratio = 1)
# # p + coord_polar_cartesian()






