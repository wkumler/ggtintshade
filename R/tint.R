# Core tinting engine. Turns a built layer's mapped hue(s) into within-group
# lightness tints and records, per tintshade value, the set of final colours the
# legend guide will need. Not exported.

# Which of `colour` / `fill` carry the hue? Kept if present and not entirely
# absent; the "active" ones are those that vary. If none varies (e.g. a constant
# colour), fall back to the constant one so a deliberate single-group ramp works.
tint_targets <- function(data) {
  present <- intersect(c("colour", "fill"), names(data))
  present <- present[vapply(present, function(a) !all(is.na(data[[a]])), logical(1))]
  if (length(present) == 0) {
    return(character(0))
  }
  n <- vapply(present, function(a) length(unique(stats::na.omit(data[[a]]))), integer(1))
  active <- present[n >= 2]
  if (length(active) == 0) present else active
}

# Tint the hue aesthetic(s) of `data` and fill `cache`. `self` is the geom, used
# only for cli messages.
tint_layer <- function(data, cache, self) {
  targets <- tint_targets(data)
  if (length(targets) == 0) {
    return(data)
  }
  n_tint <- length(unique(stats::na.omit(data$tintshade)))

  for (aes in targets) {
    hue <- data[[aes]]
    t <- local_tint(hue, data$tintshade)

    if (n_tint > 1 && length(unique(stats::na.omit(hue))) == 1) {
      cli::cli_inform(c(
        "{.fn {snake_class(self)}}: all {.field tintshade} values fall within a single {.field {aes}} group.",
        i = "The tint ramp spans one group globally. Map {.field {aes}} to a grouping variable for per-group ramps."
      ))
    }
    if (any(t < 0 | t > 1, na.rm = TRUE)) {
      cli::cli_warn(c(
        "{.fn {snake_class(self)}}: some {.field tintshade} values fell outside {.val {c(0, 1)}} and were clamped.",
        i = "Set {.code scale_tintshade_*(range = )} within [0, 1]."
      ))
    }

    ok <- !is.na(t) & !is.na(hue)
    final <- hue
    final[ok] <- tint(hue[ok], t[ok])
    data[[aes]] <- final

    # Record only tintshade value -> tinted hex. Nested values map to one hue
    # (unambiguous); crossed values map to several, and the guide renders those
    # greyscale anyway, so an arbitrary representative is fine.
    m <- cache$colours[[aes]] %||% character(0)
    m[cache_key(data$tintshade[ok])] <- final[ok]
    cache$colours[[aes]] <- m
  }
  data
}
