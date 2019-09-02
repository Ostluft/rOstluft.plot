#' @rdname geom_bar_wind
#' @export
stat_summary_wind <- function (data = NULL, mapping = NULL, geom = "bar_wind", position = "stack",
                               ...,
                               groups = c(),
                               fun = "mean",
                               fun.args = list(),
                               nmin = 3,
                               wd_cutfun = cut_wd.fun(binwidth = 45),
                               ws_cutfun = cut_ws.fun(binwidth = 1),
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = StatSummaryWind,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      groups = groups,
      fun = fun,
      fun.args = fun.args,
      nmin = nmin,
      wd_cutfun = wd_cutfun,
      ws_cutfun = ws_cutfun,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname rOstluft-ggproto
#' @export
StatSummaryWind <- ggproto("StatSummaryWind", Stat,
  compute_group = function(data, scales, fun = "mean", fun.args = list(), nmin = 3,
                           wd_cutfun = cut_wd.fun(binwidth = 45),
                           ws_cutfun = cut_ws.fun(binwidth = 1, ws_max = NA), groups = c()) {

    summary_wind(data = data, wd = "wd", ws = "ws", z = "z", fun = fun, fun.args = fun.args, nmin = nmin,
                  wd_cutfun = wd_cutfun, ws_cutfun = ws_cutfun, groups = groups)
  },
  required_aes = c("wd", "ws", "z")
)
