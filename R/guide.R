# A legend guide that recolours its keys to match the tinted layer. The geom
# hands over only a `tintshade value -> tinted hex` cache; the guide decides how
# each key should read (crossed vs nested, NA breaks, and the `type` override).
GuideTintshade <- ggplot2::ggproto(
  "GuideTintshade", ggplot2::GuideLegend,

  # Register the extra params so new_guide() accepts them.
  params = c(
    ggplot2::GuideLegend$params,
    list(tintshade_type = "auto", tintshade_cache = NULL, tintshade_crossed = NULL)
  ),

  # Grab the layer cache, and work out (from the raw, pre-mapping layer data)
  # which tintshade levels are "crossed" -- co-occur with more than one hue. The
  # guide sees this data before scales map, so it needs no signal from the geom.
  get_layer_key = function(self, params, layers, data, theme = NULL) {
    params <- ggplot2::ggproto_parent(ggplot2::GuideLegend, self)$get_layer_key(
      params, layers, data, theme
    )
    for (i in seq_along(layers)) {
      cache <- layers[[i]]$geom$tintshade_cache
      if (is.null(cache)) {
        next
      }
      params$tintshade_cache <- cache
      params$tintshade_crossed <- crossed_by_value(data[[i]], params$key)
      break
    }
    params
  },

  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {
    cache <- params$tintshade_cache
    if (!is.null(cache) && length(cache$colours) > 0) {
      type <- params$tintshade_type %||% "auto"
      crossed <- params$tintshade_crossed %||% list()
      for (i in seq_along(params$decor)) {
        params$decor[[i]]$data <- recolour_key(
          params$decor[[i]]$data, cache, crossed, type
        )
      }
    }
    ggplot2::ggproto_parent(ggplot2::GuideLegend, self)$draw(
      theme, position, direction, params
    )
  }
)

# For each hue aesthetic present in the raw layer data, is each tintshade level
# crossed (co-occurs with >1 hue)? Returned as named logical vectors keyed by
# the tintshade value (via cache_key()), using the scale `key` to translate the
# raw labels to their mapped values.
crossed_by_value <- function(raw, key) {
  out <- list()
  if (is.null(raw$tintshade) || is.null(key$tintshade)) {
    return(out)
  }
  label_to_key <- stats::setNames(cache_key(key$tintshade), as.character(key$.value))
  tint_label <- as.character(raw$tintshade)
  for (aes in intersect(c("colour", "fill"), names(raw))) {
    n_hue <- tapply(as.character(raw[[aes]]), tint_label,
                    function(x) length(unique(x)))
    vk <- label_to_key[names(n_hue)]
    keep <- !is.na(vk)
    out[[aes]] <- stats::setNames(unname(n_hue[keep] > 1), vk[keep])
  }
  out
}

# Colour each legend key, keyed throughout by the mapped tintshade value:
#   NA break -> untinted middle (neutral grey), matching the panel's NA points
#   crossed  -> neutral grey at the key's lightness (auto/crossed)
#   nested   -> the cached tinted hue
recolour_key <- function(key, cache, crossed, type) {
  if (is.null(key$tintshade)) {
    return(key)
  }
  vk <- cache_key(key$tintshade)
  for (aes in names(cache$colours)) {
    if (is.null(key[[aes]])) {
      next
    }
    orig <- as.character(key[[aes]])
    key[[aes]] <- vapply(seq_along(vk), function(i) {
      val <- key$tintshade[i]
      if (is.na(val)) {
        return(neutral_tint(0.5))
      }
      hex <- cache$colours[[aes]][[vk[i]]]
      is_crossed <- isTRUE(crossed[[aes]][[vk[i]]])
      if (is.null(hex)) {
        orig[i]
      } else if (type == "crossed" || (type == "auto" && is_crossed)) {
        neutral_tint(val)
      } else {
        hex
      }
    }, character(1))
  }
  key
}

#' Legend guide for the tintshade aesthetic
#'
#' A [ggplot2::guide_legend()] whose keys are recoloured to match the tinted
#' layer. The tintshade scale uses this guide by default.
#'
#' @param title Legend title. Defaults to the mapped variable name.
#' @param type How to colour the legend keys:
#'   * `"auto"` (default): a key is drawn in its hue when its tintshade level
#'     maps to a single hue (a nested design) and in neutral grey when it
#'     co-occurs with several (a crossed design, where lightness is
#'     hue-independent).
#'   * `"unique"`: always use the hue.
#'   * `"crossed"`: always use neutral grey.
#' @param ... Passed to [ggplot2::guide_legend()].
#'
#' @return A guide object.
#' @seealso [scale_tintshade_discrete()]
#' @export
#' @examples
#' library(ggplot2)
#' df <- expand.grid(hue = c("good", "bad"), level = c("lo", "hi"))
#' df$y <- 1
#' ggplot(df, aes(hue, y, fill = hue, tintshade = level)) +
#'   geom_col_tintshade(position = "dodge") +
#'   guides(tintshade = guide_tintshade(type = "crossed"))
guide_tintshade <- function(title = ggplot2::waiver(),
                            type = c("auto", "unique", "crossed"), ...) {
  type <- match.arg(type)
  ggplot2::new_guide(
    title = title, tintshade_type = type, available_aes = "tintshade",
    name = "tintshade", super = GuideTintshade
  )
}
