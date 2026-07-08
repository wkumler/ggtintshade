#' Geoms with a tintshade aesthetic
#'
#' @description
#' Drop-in tintshade variants of common ggplot2 geoms. Map `colour` or `fill`
#' to a hue and `tintshade` to a within-hue lightness; whichever of
#' `colour`/`fill` carries the hue is tinted automatically (both, if both are
#' mapped). The plotted layer and its legend stay in sync.
#'
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... Standard
#'   [ggplot2::layer()] arguments, with the same defaults as the corresponding
#'   plain ggplot2 geom.
#'
#' @return A ggplot2 layer.
#' @seealso [scale_tintshade_discrete()], [guide_tintshade()]
#' @name geom_tintshade
#' @examples
#' library(ggplot2)
#' df <- data.frame(
#'   g = rep(c("a", "b", "c"), each = 3),
#'   item = rep(c("lo", "mid", "hi"), 3),
#'   x = rep(1:3, each = 3), y = runif(9)
#' )
#' ggplot(df, aes(x, y, colour = g, tintshade = item)) +
#'   geom_point_tintshade(size = 4)
NULL

# --- internal geom objects (the factory calls) -------------------------------

GeomPointTintshade      <- make_tintshade_geom(ggplot2::GeomPoint)
GeomLineTintshade       <- make_tintshade_geom(ggplot2::GeomLine)
GeomPathTintshade       <- make_tintshade_geom(ggplot2::GeomPath)
GeomStepTintshade       <- make_tintshade_geom(ggplot2::GeomStep)
GeomAreaTintshade       <- make_tintshade_geom(ggplot2::GeomArea)
GeomRibbonTintshade     <- make_tintshade_geom(ggplot2::GeomRibbon)
GeomBarTintshade        <- make_tintshade_geom(ggplot2::GeomBar)
GeomColTintshade        <- make_tintshade_geom(ggplot2::GeomCol)
GeomBoxplotTintshade    <- make_tintshade_geom(ggplot2::GeomBoxplot)
GeomViolinTintshade     <- make_tintshade_geom(ggplot2::GeomViolin)
GeomTileTintshade       <- make_tintshade_geom(ggplot2::GeomTile)
GeomRasterTintshade     <- make_tintshade_geom(ggplot2::GeomRaster)
GeomRectTintshade       <- make_tintshade_geom(ggplot2::GeomRect)
GeomPolygonTintshade    <- make_tintshade_geom(ggplot2::GeomPolygon)
GeomSegmentTintshade    <- make_tintshade_geom(ggplot2::GeomSegment)
GeomTextTintshade       <- make_tintshade_geom(ggplot2::GeomText)
GeomLabelTintshade      <- make_tintshade_geom(ggplot2::GeomLabel)
GeomCrossbarTintshade   <- make_tintshade_geom(ggplot2::GeomCrossbar)
GeomErrorbarTintshade   <- make_tintshade_geom(ggplot2::GeomErrorbar)
GeomLinerangeTintshade  <- make_tintshade_geom(ggplot2::GeomLinerange)
GeomPointrangeTintshade <- make_tintshade_geom(ggplot2::GeomPointrange)

# --- exported constructors (the factory calls) -------------------------------

#' @rdname geom_tintshade
#' @export
geom_point_tintshade <- make_geom_constructor(GeomPointTintshade, "identity", "identity")

#' @rdname geom_tintshade
#' @export
geom_jitter_tintshade <- make_geom_constructor(GeomPointTintshade, "identity", "jitter")

#' @rdname geom_tintshade
#' @export
geom_line_tintshade <- make_geom_constructor(GeomLineTintshade, "identity", "identity")

#' @rdname geom_tintshade
#' @export
geom_path_tintshade <- make_geom_constructor(GeomPathTintshade, "identity", "identity")

#' @rdname geom_tintshade
#' @export
geom_step_tintshade <- make_geom_constructor(GeomStepTintshade, "identity", "identity")

#' @rdname geom_tintshade
#' @export
geom_area_tintshade <- make_geom_constructor(GeomAreaTintshade, "align", "stack")

#' @rdname geom_tintshade
#' @export
geom_ribbon_tintshade <- make_geom_constructor(GeomRibbonTintshade, "identity", "identity")

#' @rdname geom_tintshade
#' @export
geom_bar_tintshade <- make_geom_constructor(GeomBarTintshade, "count", "stack")

#' @rdname geom_tintshade
#' @export
geom_col_tintshade <- make_geom_constructor(GeomColTintshade, "identity", "stack")

#' @rdname geom_tintshade
#' @export
geom_histogram_tintshade <- make_geom_constructor(GeomBarTintshade, "bin", "stack")

#' @rdname geom_tintshade
#' @export
geom_boxplot_tintshade <- make_geom_constructor(GeomBoxplotTintshade, "boxplot", "dodge2")

#' @rdname geom_tintshade
#' @export
geom_violin_tintshade <- make_geom_constructor(GeomViolinTintshade, "ydensity", "dodge")

#' @rdname geom_tintshade
#' @export
geom_tile_tintshade <- make_geom_constructor(GeomTileTintshade, "identity", "identity")

#' @rdname geom_tintshade
#' @export
geom_raster_tintshade <- make_geom_constructor(GeomRasterTintshade, "identity", "identity")

#' @rdname geom_tintshade
#' @export
geom_rect_tintshade <- make_geom_constructor(GeomRectTintshade, "identity", "identity")

#' @rdname geom_tintshade
#' @export
geom_polygon_tintshade <- make_geom_constructor(GeomPolygonTintshade, "identity", "identity")

#' @rdname geom_tintshade
#' @export
geom_segment_tintshade <- make_geom_constructor(GeomSegmentTintshade, "identity", "identity")

#' @rdname geom_tintshade
#' @export
geom_text_tintshade <- make_geom_constructor(GeomTextTintshade, "identity", "identity")

#' @rdname geom_tintshade
#' @export
geom_label_tintshade <- make_geom_constructor(GeomLabelTintshade, "identity", "identity")

#' @rdname geom_tintshade
#' @export
geom_crossbar_tintshade <- make_geom_constructor(GeomCrossbarTintshade, "identity", "identity")

#' @rdname geom_tintshade
#' @export
geom_errorbar_tintshade <- make_geom_constructor(GeomErrorbarTintshade, "identity", "identity")

#' @rdname geom_tintshade
#' @export
geom_linerange_tintshade <- make_geom_constructor(GeomLinerangeTintshade, "identity", "identity")

#' @rdname geom_tintshade
#' @export
geom_pointrange_tintshade <- make_geom_constructor(GeomPointrangeTintshade, "identity", "identity")
