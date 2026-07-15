
set.seed(123)
base_ggplot_colors <- scales::hue_pal()(3)

metab_data <- data.frame(
  metab = rep(c("Alanine", "Threonine", "Glycine",
                "Glycine betaine", "Proline betaine", "Carnitine",
                "DMSP", "DMS-Ac", "Isethionate"), 3),
  metab_group = rep(rep(c("Amino acid", "Betaine", "Sulfur"), each = 3), 3),
  tripl = rep(c("A", "B", "C"), each = 9),
  area  = runif(27)
)
gp <- ggplot(metab_data) +
  geom_point_tintshade(aes(x=tripl, y=area, color=metab_group, tintshade = metab))
built_gp <- ggplot_build(gp)

test_that("ggtintshade renders", {
  expect_doppelganger("init", gp)
})

test_that("Colors tint correctly", {
  lightened <- colorspace::lighten(base_ggplot_colors, 2 * 0.8 - 1)
  darkened <- colorspace::lighten(base_ggplot_colors, 2 * 0.2 - 1)
  expect_setequal(built_gp@data[[1]]$colour, c(base_ggplot_colors, lightened, darkened))
})

test_that("Other aes identical", {
  base_gp <- ggplot(metab_data) +
    geom_point(aes(x=tripl, y=area, color=metab_group))
  built_base <- ggplot_build(base_gp)

  expect_identical(built_base@data$x, built_gp@data$x)
  expect_identical(built_base@data$y, built_gp@data$y)
  expect_identical(built_base@data$PANEL, built_gp@data$PANEL)
  expect_identical(built_base@data$group, built_gp@data$group)
  expect_identical(built_base@data$shape, built_gp@data$shape)
  expect_identical(built_base@data$fill, built_gp@data$fill)
  expect_identical(built_base@data$size, built_gp@data$size)
  expect_identical(built_base@data$alpha, built_gp@data$alpha)
  expect_identical(built_base@data$stroke, built_gp@data$stroke)
})

test_that("Scale tints differently", {
  built_gp <- ggplot_build(gp + scale_tintshade_discrete(range = c(0.1, 0.9)))
  lightened <- colorspace::lighten(base_ggplot_colors, 2 * 0.9 - 1)
  darkened <- colorspace::lighten(base_ggplot_colors, 2 * 0.1 - 1)
  expect_setequal(built_gp@data[[1]]$colour, c(base_ggplot_colors, lightened, darkened))
})

test_that("0-1 scale means black/white", {
  built_gp <- ggplot_build(gp + scale_tintshade_discrete(range = c(0, 1)))
  expect_contains(built_gp@data[[1]]$colour, "#000000")
  expect_contains(built_gp@data[[1]]$colour, "#FFFFFF")
})

test_that("other geoms work", {
  base_gp <- ggplot(metab_data) + aes(x=tripl, y=area, color=metab_group, fill=metab_group, tintshade = metab, group=metab)
  expect_doppelganger("point", base_gp + geom_point_tintshade())
  expect_doppelganger("line", base_gp + geom_line_tintshade())
  expect_doppelganger("path", base_gp + geom_path_tintshade(data = metab_data[sample(seq_len(nrow(metab_data))),]))
  expect_doppelganger("polygon", base_gp + geom_polygon_tintshade())
  expect_doppelganger("step", base_gp + geom_step_tintshade())
  expect_doppelganger("area", base_gp + geom_area_tintshade())
  expect_doppelganger("ribbon", base_gp + geom_ribbon_tintshade(aes(ymin=area-0.1, ymax=area+0.1)))
  expect_doppelganger("bar", ggplot(metab_data) + geom_bar_tintshade(aes(x=tripl, fill=metab_group, tintshade = metab)))
  expect_doppelganger("col", base_gp + geom_col_tintshade())
  expect_doppelganger("boxplot", base_gp + geom_boxplot_tintshade())
  expect_doppelganger("violin", base_gp + geom_violin_tintshade())
  expect_doppelganger("tile", base_gp + geom_tile_tintshade(width=0.5, height=0.1))
  expect_doppelganger("rect", base_gp + geom_rect_tintshade(aes(ymin=area-0.1, ymax=area+0.1), width=0.5))
  expect_doppelganger("raster", base_gp + geom_raster_tintshade(aes(y=round(area*10))))
  expect_doppelganger("segment", base_gp + geom_segment_tintshade(xend=1.5, yend=0.5, arrow=arrow(ends="first", type="closed")))
  expect_doppelganger("text", base_gp + geom_text_tintshade(aes(label=metab)))
  expect_doppelganger("label", base_gp + geom_label_tintshade(aes(label=metab), fill="white"))
})

test_that("vignette demos work", {
  metab_data <- data.frame(
    metab = rep(c("Alanine", "Threonine", "Glycine",
                  "Glycine betaine", "Proline betaine", "Carnitine",
                  "DMSP", "DMS-Ac", "Isethionate"), 3),
    metab_group = rep(rep(c("Amino acid", "Betaine", "Sulfur"), each = 3), 3),
    tripl = rep(c("A", "B", "C"), each = 9),
    area  = runif(27)
  )
  metab_data$metab <- factor(metab_data$metab, levels = unique(metab_data$metab))
  gp <- ggplot(metab_data) +
    geom_col_tintshade(aes(x=tripl, y=area, fill=metab_group, tintshade = metab))
  expect_doppelganger("readme_metab", gp)

  align_data <- data.frame(
    alignment=1:9,
    moral=rep(c("good", "neutral", "evil"), each=3),
    meta=rep(c("lawful", "neutral", "chaotic"), length.out=9)
  )
  align_data$moral <- factor(align_data$moral, levels=rev(unique(align_data$moral)))
  align_data$meta <- factor(align_data$meta, levels=unique(align_data$meta))
  gp <- ggplot(align_data) +
    geom_raster_tintshade(aes(x=meta, y=moral, fill=meta, tintshade=moral)) +
    scale_fill_manual(breaks = c("lawful", "neutral", "chaotic"), values=c("#cca40a", "#b52060", "#7623b2")) +
    coord_equal()
  expect_doppelganger("readme_align", gp)

  grp <- c(I1 = "I", SI2 = "SI", SI1 = "SI", VS2 = "VS", VS1 = "VS", VVS2 = "VVS", VVS1 = "VVS", IF = "IF")
  diamonds$clarity_group <- factor(grp[as.character(diamonds$clarity)], levels = c("I", "SI", "VS", "VVS", "IF"))
  mp <- aggregate(price ~ clarity + clarity_group, diamonds, mean)
  nested_gp <- ggplot(mp) +
    geom_col_tintshade(aes(clarity, price, fill = clarity_group, tintshade = clarity)) +
    scale_tintshade_discrete(range = c(0.4, 0.6)) +
    ggtitle("Nested diamonds") +
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5))
  expect_doppelganger("readme_nested_diamonds", nested_gp)
  crossed_gp <- ggplot(diamonds) +
    geom_bar_tintshade(aes(x=cut, fill = cut, tintshade = clarity), color="black") +
    ggtitle("Crossed diamonds") +
    scale_tintshade_discrete(range = c(0.1, 0.9)) +
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5))
  expect_doppelganger("readme_crossed_diamonds", crossed_gp)

  temp.data = data.frame (
    Species  = rep(c("A","B"),each=2, times=2),
    Status = rep(c("An","Bac"), times=4),
    Sex = rep(c("Male","Female"), each=4, times=1),
    Proportion = c(6.86, 7.65, 30.13, 35.71, 7.13, 10.33, 29.24, 31.09)
  )
  new_tinted <- ggplot(temp.data, aes(x = Species, y = Proportion, fill = Species, tintshade = Status)) +
    geom_bar_tintshade(stat='identity', position = position_dodge(width = 0.73), width=.67) +
    facet_grid(Sex ~ .) +
    scale_fill_manual(name = "Status", labels = c("An","Bac"), values = c("#cf944c","#0a3e03")) +
    scale_tintshade_discrete(range = c(0.3, 0.7))
  expect_doppelganger("readme_alpha_replace", new_tinted)

  d <- data.frame(
    x=rep(1:20, 5), y=rnorm(100, 5, .2) + rep(1:5, each=20),
    z=rep(1:20, 5), grp=factor(rep(1:5, each=20))
  )
  new_tinted <- ggplot(d) +
    geom_path_tintshade(aes(x, y, color=grp, tintshade=z), linewidth=2, lineend=0) +
    scale_tintshade_continuous(range = c(0.5, 0))
  expect_doppelganger("readme_continuous_alpha", new_tinted)

  mpgsub <- head(mpg, 60)
  mpgsub$model <- factor(mpgsub$model, levels=unique(mpgsub$model))
  gp <- ggplot(mpgsub, aes(displ, hwy, colour = manufacturer, tintshade = model)) +
    geom_point_tintshade(size = 3)
  expect_doppelganger("readme_mpg", gp)

  gp <- ggplot(penguins[!is.na(penguins$bill_len),]) +
    geom_point_tintshade(aes(x=bill_len, y=bill_dep, fill=species, tintshade=sex),
                         pch=21, color="black", size=3)
  expect_doppelganger("readme_penguins", gp)
})

test_that("internals vignette works", {
  metab_data <- data.frame(
    metab = rep(c("Alanine", "Threonine", "Glycine",
                  "Glycine betaine", "Proline betaine", "Carnitine",
                  "DMSP", "DMS-Ac", "Isethionate"), 3),
    metab_group = rep(rep(c("Amino acid", "Betaine", "Sulfur"), each = 3), 3),
    tripl = rep(c("A", "B", "C"), each = 9),
    area  = runif(27)
  )

  init_gp <- ggplot(metab_data) +
    aes(x = tripl, y = area, color = metab_group, tintshade = metab) +
    geom_point_tintshade(size = 4)
  expect_doppelganger("vig_internal_init", init_gp)

  geom_point_tintshadedemo <- function(mapping = NULL, data = NULL, ..., size = NULL) {
    cache <- new.env(parent = emptyenv())
    cache$lookup <- character(0)

    geom <- ggproto(NULL, GeomPointTintshadeDemo, tintshade_cache = cache)
    layer(
      geom = geom, mapping = mapping, data = data,
      stat = "identity", position = "identity",
      params = list(size = size, ...)
    )
  }
  # Rank tintshade values within each color group and spread over the range.
  local_tint <- function(color, tintshade) {
    ave(tintshade, color, FUN = function(v) {
      ranks <- match(v, sort(unique(v)))
      scales::rescale(ranks, to = range(tintshade))
    })
  }

  GeomPointTintshadeDemo <- ggproto("GeomPointTintshadeDemo", GeomPoint,
                                    tintshade_cache = NULL,   # supplied by the constructor
                                    use_defaults = function(self, data, params = list(), ...) {
                                      data <- ggproto_parent(GeomPoint, self)$use_defaults(data, params, ...)

                                      if (!is.null(self$tintshade_cache) && is.null(data$.id) && !is.null(data$tintshadedemo)) {
                                        t <- local_tint(data$colour, data$tintshadedemo)
                                        data$colour <- colorspace::lighten(data$colour, 2 * t - 1)   # 0.5 -> no change
                                        self$tintshade_cache$lookup[sprintf("%.10f", data$tintshadedemo)] <- data$colour
                                      }
                                      data
                                    }
  )
  GeomPointTintshadeDemo$default_aes$tintshadedemo <- NA   # register the new aesthetic

  scale_tintshadedemo_discrete <- function(..., range = c(0.2, 0.8)) {
    discrete_scale("tintshadedemo", palette = function(n) seq(range[1], range[2], length.out = n))
  }
  badlegend_gp <- ggplot(metab_data) +
    aes(x = tripl, y = area, color = metab_group, tintshadedemo = metab) +
    geom_point_tintshadedemo(size = 4)
  expect_doppelganger("vig_internal_badlegend", badlegend_gp)

  GuideTintshadeDemo <- ggproto("GuideTintshadeDemo", GuideLegend,
                                get_layer_key = function(self, params, layers, data, theme = NULL) {
                                  params <- ggproto_parent(GuideLegend, self)$get_layer_key(params, layers, data, theme)
                                  params$tintshade_cache <- layers[[1]]$geom$tintshade_cache
                                  params
                                },
                                draw = function(self, theme, position = NULL, direction = NULL, params = self$params) {
                                  keys <- params$decor[[1]]$data
                                  params$decor[[1]]$data$colour <-
                                    params$tintshade_cache$lookup[sprintf("%.10f", keys$tintshadedemo)]
                                  ggproto_parent(GuideLegend, self)$draw(theme, position, direction, params)
                                }
  )

  guide_tintshadedemo <- function(...) {
    new_guide(..., available_aes = "tintshadedemo", super = GuideTintshadeDemo)
  }

  scale_tintshadedemo_discrete <- function(..., range = c(0.2, 0.8)) {
    discrete_scale("tintshadedemo", palette = function(n) seq(range[1], range[2], length.out = n),
                   guide = guide_tintshadedemo())
  }
  final_gp <- ggplot(metab_data) +
    aes(x = tripl, y = area, color = metab_group, tintshadedemo = metab) +
    geom_point_tintshadedemo(size = 4)
  expect_doppelganger("vig_internal_final", final_gp)
})
