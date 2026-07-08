# Factories that build the tintshade geoms and their constructors. The factory
# *calls* live in geoms.R; this file is just the machinery. Not exported.

# Wrap a base ggplot2 geom so its hue aesthetic is tinted at build time.
make_tintshade_geom <- function(base_geom) {
  geom <- ggplot2::ggproto(
    paste0(class(base_geom)[1], "Tintshade"), base_geom,
    tintshade_cache = NULL,  # a per-layer instance supplies the real cache
    tintshade_base = base_geom,
    use_defaults = function(self, data, params = list(), ...) {
      data <- ggplot2::ggproto_parent(self$tintshade_base, self)$use_defaults(
        data, params = params, ...
      )
      cache <- self$tintshade_cache
      # Tint real panel data only: legend keys carry a `.id` column, and a layer
      # may not map tintshade at all (the column is then all-NA).
      if (!is.null(cache) && is.null(data[[".id"]]) &&
          is.numeric(data$tintshade) && !all(is.na(data$tintshade))) {
        data <- tint_layer(data, cache, self)
      }
      data
    }
  )
  geom$default_aes$tintshade <- NA
  geom
}

# Build a user-facing geom constructor for a tintshade geom. Each call mints a
# fresh geom instance with its own cache, so layers never share state. The
# `stat`/`position` defaults are baked into the returned function's formals so
# that generated documentation shows their real values.
make_geom_constructor <- function(geom, default_stat, default_position) {
  force(geom)
  fun <- function(mapping = NULL, data = NULL, stat, position, ..., na.rm = FALSE,
                  show.legend = NA, inherit.aes = TRUE) {
    instance <- ggplot2::ggproto(NULL, geom, tintshade_cache = new_tint_cache())
    ggplot2::layer(
      geom = instance, mapping = mapping, data = data, stat = stat,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  }
  formals(fun)$stat <- default_stat
  formals(fun)$position <- default_position
  fun
}
