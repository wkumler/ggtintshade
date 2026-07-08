# Small internal helpers. None are exported.

`%||%` <- function(x, y) if (is.null(x)) y else x

# Convert any R colour spec ("red", "#FF0000") to a plain "#RRGGBB" hex.
to_hex <- function(x) {
  m <- grDevices::col2rgb(x)
  grDevices::rgb(m[1, ], m[2, ], m[3, ], maxColorValue = 255)
}

# Tint a base colour by `t` in [0, 1]: 0 = black, 0.5 = the base colour,
# 1 = white. Values outside [0, 1] are clamped so an out-of-range scale cannot
# overflow into spurious hues. Uses perceptual lightness, which keeps hue fixed
# and behaves the same across colours.
tint <- function(base, t) {
  colorspace::lighten(to_hex(base), pmax(-1, pmin(1, 2 * t - 1)))
}

# A neutral, hueless tint used for crossed-design legend keys.
neutral_tint <- function(t) {
  tint("grey50", t)
}

# Per-row local tint value: rank each row's tintshade value among the rows
# sharing its hue, then spread those ranks across the layer-wide tintshade
# range so every hue gets its own dark -> pale sweep. A lone value maps to 0.5
# (its untinted base colour).
local_tint <- function(hue, tintshade) {
  span <- range(tintshade, na.rm = TRUE)
  if (!all(is.finite(span)) || diff(span) == 0) {
    return(rep(0.5, length(tintshade)))
  }
  stats::ave(tintshade, hue, FUN = function(v) {
    if (length(unique(v)) <= 1) {
      rep(0.5, length(v))
    } else {
      scales::rescale(match(v, sort(unique(v))), to = span)
    }
  })
}

# Stable string key for a (double) tintshade value.
cache_key <- function(x) {
  sprintf("%.10f", x)
}

# A fresh, layer-local cache. `sets[[aes]][[key]]` collects the distinct final
# colours seen for each tintshade value, per hue aesthetic.
new_tint_cache <- function() {
  cache <- new.env(parent = emptyenv())
  cache$sets <- list()
  cache
}

# "GeomPointTintshade" -> "geom_point_tintshade", for cli messages.
snake_class <- function(x) {
  tolower(gsub("([a-z0-9])([A-Z])", "\\1_\\2", class(x)[1]))
}
