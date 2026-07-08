#' @md
#' @details
#' The `ggtintshade` package is designed to provide an additional aesthetic to
#' typical `ggplot2` plots that allows colors to be lightened/darkened by
#' mapping the saturation/value coordinates as well as the hue. This allows for
#' grouped, nested, and crossed designs where not all colors should be equally
#' distinct.
#'
#' @keywords internal
"_PACKAGE"

# All R code references ggplot2 with `ggplot2::` so that top-level object
# creation works before this import is registered; the import makes ggplot2
# available to the test namespace and documents the dependency.
#' @import ggplot2
NULL
