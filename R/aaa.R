# Name ggplot grid object
# Convenience function to name grid objects
#
# @keyword internal
ggname <- function(prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}

#' Interpreter for graphical parameters
#'
#' This is a wrapper for [`grid::gpar()`] that applies ggplot2's interpretation
#' of graphical parameters.
#'
#' @param ... Named arguments passed on to `gpar()`.
#' @param stroke Linewidth for points. Populates the `lwd` grid parameter.
#' @param pointsize Size for points. Populates the `fontsize` grid parameter.
#'
#' @return An object of class 'gpar'.
#' @keywords internal
#' @export
gg_par <- function(..., stroke = NULL, pointsize = NULL) {
  args <- list2(...)
  args <- args[lengths(args) > 0]

  if (!is.null(args$lwd)) {
    args$lwd <- args$lwd * .pt
  }
  if (!is.null(stroke)) {
    args$lwd <- stroke * .stroke / 2
  }
  if (!is.null(pointsize)) {
    # Stroke is added around the outside of the point
    stroke <- stroke %||% 0
    stroke[is.na(stroke)] <- 0
    args$fontsize <- pointsize * .pt + stroke * .stroke / 2
  }

  inject(gpar(!!!args))
}
