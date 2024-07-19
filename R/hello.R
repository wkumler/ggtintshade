
#' @export
geom_point_tintshade <- function(mapping = NULL, data = NULL,
                                 stat = "identity", position = "identity",
                                 ...,
                                 na.rm = FALSE,
                                 show.legend = NA,
                                 inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPointTintshade,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      ...
    )
  )
}

#' @export
GeomPointTintshade <- ggproto("GeomPointTintshade", GeomPoint,
                              required_aes = c("x", "y"),
                              non_missing_aes = c("size", "shape", "colour", "tintshade"),
                              default_aes = aes(
                                shape = 19, colour = "black", size = 1.5, fill = NA,
                                alpha = NA, stroke = 0.5, tintshade=0.5
                              ),

                              draw_panel = function(self, data, panel_params, coord, na.rm = FALSE) {

                                coords <- coord$transform(data, panel_params)
                                # assign("coords", coords, envir = .GlobalEnv)
                                coords$colour <- lighten(coords$colour, amount = (coords$tintshade*2)-1)

                                ggname("geom_point_tintshade",
                                       pointsGrob(
                                         coords$x, coords$y,
                                         pch = coords$shape,
                                         gp = gg_par(
                                           col = alpha(coords$colour, coords$alpha),
                                           fill = fill_alpha(coords$fill, coords$alpha),
                                           pointsize = coords$size,
                                           stroke = coords$stroke
                                         )
                                       )
                                )
                              },

                              draw_key = function(self, ...){
                                draw_key_point_tintshade(...)
                              }
)

#' @export
scale_tintshade_continuous <- function(name = waiver(), ..., range = c(0.2, 0.8)) {
  continuous_scale("tintshade", name = name, palette = pal_rescale(range), ...)
}

#' @export
scale_tintshade_discrete <- function(name = waiver(), ..., range = c(0.2, 0.8)) {
  force(range)
  discrete_scale(
    "tintshade", name = name,
    palette = function(n) seq(range[1], range[2], length.out = n),
    ...
  )
}

draw_key_point_tintshade <- function(data, params, size){
  # message("Printing data")
  # print(data)
  if (is.null(data$shape)) {
    data$shape <- 19
  } else if (is.character(data$shape)) {
    data$shape <- translate_shape_string(data$shape)
  }
  stroke_size <- data$stroke %||% 0.5
  stroke_size[is.na(stroke_size)] <- 0
  # This needs to be applied per-group instead of as a whole like it is currently
  # I need a complete list of all the data available in order to do this
  tintshade_colors <- lighten(data$colour, amount = (data$tintshade*2)-1)
  pointsGrob(0.5, 0.5, pch = data$shape, gp = gpar(
    col = alpha(tintshade_colors, data$alpha),
    fill = fill_alpha(data$fill %||% "black", data$alpha),
    fontsize = (data$size %||% 1.5) * .pt + stroke_size * .stroke/2,
    lwd = stroke_size * .stroke/2))
}
