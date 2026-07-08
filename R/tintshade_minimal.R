
library(ggplot2)

# Helpers ----
to_hex <- function(x) {
  m <- col2rgb(x)
  rgb(m[1, ], m[2, ], m[3, ], maxColorValue = 255)
}

tint <- function(base, t) {
  colorspace::lighten(to_hex(base), 2 * t - 1)
}

local_tint <- function(colour, tintshade) {
  ave(tintshade, colour, FUN = function(v) {
    ranks <- match(v, sort(unique(v)))
    scales::rescale(ranks, to = range(tintshade))
  })
}

cache_key <- function(x) sprintf("%.10f", x)

# Geom ----
GeomPointTintshade <- ggproto("GeomPointTintshade", GeomPoint,
  tintshade_cache = NULL,   # each layer gets its own environment here

  # use_defaults() is called during ggplot_build(), once for the panel data
  # (mapped hue + mapped tintshade in hand) and once for the legend key.
  use_defaults = function(self, data, params = list(), ...) {
    data <- ggproto_parent(GeomPoint, self)$use_defaults(data, params, ...)

    # Only touch real panel data: the legend key carries a `.id` column, and a
    # layer might not use tintshade at all.
    if (!is.null(self$tintshade_cache) &&
        is.null(data$.id) && !is.null(data$tintshade)) {

      # 1. tint each point and overwrite its colour
      t <- local_tint(data$colour, data$tintshade)
      data$colour <- tint(data$colour, t)

      # 2. remember tintshade value -> final colour for the legend
      self$tintshade_cache$lookup[cache_key(data$tintshade)] <- data$colour
    }
    data
  }
)
GeomPointTintshade$default_aes$tintshade <- NA   # register the new aesthetic

geom_point_tintshade <- function(mapping = NULL, data = NULL, ..., size = NULL) {
  # One fresh cache per layer -> no shared or global state.
  cache <- new.env(parent = emptyenv())
  cache$lookup <- character(0)

  geom <- ggproto(NULL, GeomPointTintshade, tintshade_cache = cache)
  layer(
    geom = geom, mapping = mapping, data = data,
    stat = "identity", position = "identity",
    params = list(size = size, ...)
  )
}

# Guide + scale ----
GuideTintshade <- ggproto("GuideTintshade", GuideLegend,

  # Grab this layer's cache while the legend is being prepared.
  get_layer_key = function(self, params, layers, data, theme = NULL) {
    params <- ggproto_parent(GuideLegend, self)$get_layer_key(params, layers, data, theme)
    params$tintshade_cache <- layers[[1]]$geom$tintshade_cache
    params
  },

  # By the time draw() runs, ggplot_build() has filled the cache, so we can look
  # up the final colour for each key's tintshade value.
  draw = function(self, theme, position = NULL, direction = NULL, params = self$params) {
    keys <- params$decor[[1]]$data
    params$decor[[1]]$data$colour <- params$tintshade_cache$lookup[cache_key(keys$tintshade)]
    ggproto_parent(GuideLegend, self)$draw(theme, position, direction, params)
  }
)

guide_tintshade <- function(...) {
  new_guide(..., available_aes = "tintshade", super = GuideTintshade)
}

scale_tintshade_discrete <- function(..., range = c(0.2, 0.8)) {
  discrete_scale(
    aesthetics = "tintshade",
    palette = function(n) seq(range[1], range[2], length.out = n),
    guide = guide_tintshade()
  )
}

# Repeated for boxplots ----
GeomBoxplotTintshade <- ggproto(
  "GeomBoxplotTintshade", GeomBoxplot,
  tintshade_cache = NULL,
  use_defaults = function(self, data, params = list(), ...) {
    data <- ggproto_parent(GeomBoxplot, self)$use_defaults(data, params, ...)
    if (!is.null(self$tintshade_cache) && is.null(data$.id) && !is.null(data$tintshade)) {
      t <- local_tint(data$colour, data$tintshade)
      data$colour <- tint(data$colour, t)
      self$tintshade_cache$lookup[cache_key(data$tintshade)] <- data$colour
    }
    data
  }
)
GeomBoxplotTintshade$default_aes$tintshade <- NA

geom_boxplot_tintshade <- function(mapping = NULL, data = NULL, ..., size = NULL) {
  cache <- new.env(parent = emptyenv())
  cache$lookup <- character(0)
  geom <- ggproto(NULL, GeomBoxplot, tintshade_cache = cache)
  layer(
    geom = geom, mapping = mapping, data = data,
    stat = "identity", position = "identity",
    params = list(size = size, ...)
  )
}

# Reprex ----
metab_data <- data.frame(
  metab = rep(c("Alanine", "Threonine", "Glycine",
                "Glycine betaine", "Proline betaine", "Carnitine",
                "DMSP", "DMS-Ac", "Isethionate"), 3),
  metab_group = rep(rep(c("Amino acid", "Betaine", "Sulfur"), each = 3), 3),
  tripl = rep(c("A", "B", "C"), each = 9),
  area  = runif(27)
)

ggplot(metab_data) +
  geom_point_tintshade(
    aes(x = tripl, y = area, colour = metab_group, tintshade = metab),
    size = 4
  )
metab_data$metab <- factor(metab_data$metab, levels = unique(metab_data$metab))
ggplot(metab_data) +
  geom_point_tintshade(
    aes(x = tripl, y = area, colour = metab_group, tintshade = metab),
    size = 4
  )

ggplot(metab_data) +
  geom_boxplot_tintshade(aes(x=metab_group, y=area, color=metab_group, tintshade = metab))
