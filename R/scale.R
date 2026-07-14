#' Lightness (tintshade) scales
#'
#' Map a variable to a lightness tint. The mapped value runs from `0` (black)
#' through `0.5` (the base hue) to `1` (white); the geom rescales these within
#' each hue group so the ramp restarts per group.
#'
#' @param range Numeric length-2 output range within `[0, 1]`. Defaults to
#'   `c(0.2, 0.8)` (dark to pale, base hue in the middle).
#' @param aesthetics Aesthetic this scale applies to. Defaults to `"tintshade"`.
#' @param guide The legend guide. Defaults to [guide_tintshade()]; pass a
#'   configured `guide_tintshade(...)` to customise the legend.
#' @param ... Passed to [ggplot2::discrete_scale()] or
#'   [ggplot2::continuous_scale()].
#'
#' @return A ggplot2 scale object.
#' @seealso [guide_tintshade()]
#' @name scale_tintshade
#' @examples
#' library(ggplot2)
#' df <- data.frame(x = 1:3, y = 1:3, g = "a", item = c("lo", "mid", "hi"))
#' ggplot(df, aes(x, y, colour = g, tintshade = item)) +
#'   geom_point_tintshade(size = 4) +
#'   scale_tintshade_discrete(range = c(0.1, 0.9))
NULL

#' @rdname scale_tintshade
#' @export
scale_tintshade_discrete <- function(..., range = c(0.2, 0.8),
                                     aesthetics = "tintshade",
                                     guide = guide_tintshade()) {
  ggplot2::discrete_scale(
    aesthetics = aesthetics,
    palette = function(n) seq(range[1], range[2], length.out = n),
    guide = guide, ...
  )
}

#' @rdname scale_tintshade
#' @export
scale_tintshade_continuous <- function(..., range = c(0.2, 0.8),
                                       aesthetics = "tintshade",
                                       guide = guide_tintshade()) {
  ggplot2::continuous_scale(
    aesthetics = aesthetics,
    palette = function(x) scales::rescale(x, to = range),
    guide = guide, ...
  )
}
