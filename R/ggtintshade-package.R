#' @md
#' @details
#' The `ggtintshade` package is designed to provide an additional aesthetic to
#' typical `ggplot2` plots that allows colors to be lightened/darkened by
#' mapping the saturation/value coordinates as well as the hue. This allows for
#' grouped, nested, and crossed designs where not all colors should be equally
#' distinct.
#'
#' Most `ggplot2` geoms have a `tintshade` equivalent, distinguished by adding
#' the `_ggtintshade` suffix (e.g. `geom_point` -> `geom_point_tintshade`).
#' Additional geoms are easily added by editing the `geoms.R` script or by
#' submitting a Github issue (link below) with the new geom.
#'
#' @keywords internal
"_PACKAGE"

# All R code references ggplot2 with `ggplot2::` so that top-level object
# creation works before this import is registered; the import makes ggplot2
# available to the test namespace and documents the dependency.
#' @import ggplot2
NULL
