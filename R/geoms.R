#' Geoms with a tintshade aesthetic
#'
#' @description
#' Drop-in tintshade variants of common ggplot2 geoms. Map `colour` or `fill`
#' to a hue and `tintshade` to a within-hue lightness; whichever of
#' `colour`/`fill` carries the hue is tinted automatically (or both, if both are
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
#'
#' ggplot(diamonds) +
#'   geom_bar_tintshade(aes(x=cut, fill = cut, tintshade = clarity), color="black")
#'
#' mpgsub <- head(mpg, 60)
#' mpgsub$model <- factor(mpgsub$model, levels=unique(mpgsub$model))
#' ggplot(mpgsub, aes(displ, hwy, colour = manufacturer, tintshade = model)) +
#'   geom_point_tintshade(size = 3)
#'
#' ggplot(penguins[!is.na(penguins$bill_len),]) +
#'   geom_point_tintshade(aes(x=bill_len, y=bill_dep, fill=species, tintshade=sex),
#'                        pch=21, color="black", size=3)
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
geom_point_tintshade <- make_geom_constructor(GeomPointTintshade)

#' @rdname geom_tintshade
#' @export
geom_jitter_tintshade <- make_geom_constructor(GeomPointTintshade, default_position = "jitter")

#' @rdname geom_tintshade
#' @export
geom_line_tintshade <- make_geom_constructor(GeomLineTintshade)

#' @rdname geom_tintshade
#' @export
geom_path_tintshade <- make_geom_constructor(GeomPathTintshade)

#' @rdname geom_tintshade
#' @export
geom_step_tintshade <- make_geom_constructor(GeomStepTintshade)

#' @rdname geom_tintshade
#' @export
geom_area_tintshade <- make_geom_constructor(GeomAreaTintshade, default_stat = "align", default_position = "stack")

#' @rdname geom_tintshade
#' @export
geom_ribbon_tintshade <- make_geom_constructor(GeomRibbonTintshade)

#' @rdname geom_tintshade
#' @export
geom_bar_tintshade <- make_geom_constructor(GeomBarTintshade, default_stat = "count", default_position = "stack")

#' @rdname geom_tintshade
#' @export
geom_col_tintshade <- make_geom_constructor(GeomColTintshade, default_position = "stack")

#' @rdname geom_tintshade
#' @export
geom_histogram_tintshade <- make_geom_constructor(GeomBarTintshade, default_stat = "bin", default_position = "stack")

#' @rdname geom_tintshade
#' @export
geom_boxplot_tintshade <- make_geom_constructor(GeomBoxplotTintshade, default_stat = "boxplot", default_position = "dodge2")

#' @rdname geom_tintshade
#' @export
geom_violin_tintshade <- make_geom_constructor(GeomViolinTintshade, default_stat = "ydensity", default_position = "dodge")

#' @rdname geom_tintshade
#' @export
geom_tile_tintshade <- make_geom_constructor(GeomTileTintshade)

#' @rdname geom_tintshade
#' @export
geom_raster_tintshade <- make_geom_constructor(GeomRasterTintshade)

#' @rdname geom_tintshade
#' @export
geom_rect_tintshade <- make_geom_constructor(GeomRectTintshade)

#' @rdname geom_tintshade
#' @export
geom_polygon_tintshade <- make_geom_constructor(GeomPolygonTintshade)

#' @rdname geom_tintshade
#' @export
geom_segment_tintshade <- make_geom_constructor(GeomSegmentTintshade)

#' @rdname geom_tintshade
#' @export
geom_text_tintshade <- make_geom_constructor(GeomTextTintshade)

#' @rdname geom_tintshade
#' @export
geom_label_tintshade <- make_geom_constructor(GeomLabelTintshade)

#' @rdname geom_tintshade
#' @export
geom_crossbar_tintshade <- make_geom_constructor(GeomCrossbarTintshade)

#' @rdname geom_tintshade
#' @export
geom_errorbar_tintshade <- make_geom_constructor(GeomErrorbarTintshade)

#' @rdname geom_tintshade
#' @export
geom_linerange_tintshade <- make_geom_constructor(GeomLinerangeTintshade)

#' @rdname geom_tintshade
#' @export
geom_pointrange_tintshade <- make_geom_constructor(GeomPointrangeTintshade)
