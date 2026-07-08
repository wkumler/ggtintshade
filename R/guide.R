# A legend guide that recolours its keys from the layer cache at draw time.
GuideTintshade <- ggplot2::ggproto(
  "GuideTintshade", ggplot2::GuideLegend,

  # Register the extra params so new_guide() accepts them.
  params = c(
    ggplot2::GuideLegend$params,
    list(tintshade_type = "auto", tintshade_cache = NULL)
  ),

  # Capture the layer's cache reference while the legend is prepared (the cache
  # is filled later, during the geom's use_defaults).
  get_layer_key = function(self, params, layers, data, theme = NULL) {
    params <- ggplot2::ggproto_parent(ggplot2::GuideLegend, self)$get_layer_key(
      params, layers, data, theme
    )
    for (l in layers) {
      if (!is.null(l$geom$tintshade_cache)) {
        params$tintshade_cache <- l$geom$tintshade_cache
        break
      }
    }
    params
  },

  # By draw time the cache is filled, so recolour each key from it.
  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {
    cache <- params$tintshade_cache
    if (!is.null(cache) && length(cache$sets) > 0) {
      type <- params$tintshade_type %||% "auto"
      for (i in seq_along(params$decor)) {
        params$decor[[i]]$data <- recolour_key(params$decor[[i]]$data, cache, type)
      }
    }
    ggplot2::ggproto_parent(ggplot2::GuideLegend, self)$draw(
      theme, position, direction, params
    )
  }
)

# Resolve each legend key's colour/fill from the cache of seen colours.
#   auto    -> hue when the value maps to one colour, grey when it maps to many
#   unique  -> always the hue (first colour seen)
#   crossed -> always neutral grey
recolour_key <- function(key, cache, type) {
  if (is.null(key$tintshade)) {
    return(key)
  }
  k <- cache_key(key$tintshade)
  for (aes in names(cache$sets)) {
    if (is.null(key[[aes]])) {
      next
    }
    key[[aes]] <- vapply(seq_along(k), function(i) {
      set <- cache$sets[[aes]][[k[i]]]
      if (is.null(set)) {
        key[[aes]][i]
      } else if (type == "crossed" || (type == "auto" && length(set) > 1)) {
        neutral_tint(key$tintshade[i])
      } else {
        set[[1]]
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
#'   * `"auto"` (default): a key is drawn in its hue when its tintshade value
#'     maps to a single colour (a nested design) and in neutral grey when it
#'     maps to several (a crossed design, where lightness is hue-independent).
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
