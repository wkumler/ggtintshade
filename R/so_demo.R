
library(ggplot2)
library(rlang)
library(colorspace)
library(grid)
source("https://raw.githubusercontent.com/tidyverse/ggplot2/7fb4c382/R/utilities-grid.R")


geom_point_tintshade <- function(mapping = NULL, data = NULL,
                                 stat = "identity", position = "identity",
                                 ..., na.rm = FALSE, show.legend = NA,
                                 inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomPointTintshade,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list2(na.rm = na.rm, ...))
}
GeomPointTintshade <- ggproto("GeomPointTintshade", GeomPoint,
                              required_aes = c("x", "y"),
                              non_missing_aes = c("size", "shape", "colour", "tintshade"),
                              default_aes = aes(
                                shape = 19, colour = "black", size = 1.5, fill = NA,
                                alpha = NA, tintshade=0.5
                              ),
                              draw_panel = function(self, data, panel_params, coord, na.rm = FALSE) {
                                coords <- coord$transform(data, panel_params)
                                coords$colour <- lighten(coords$colour, amount = (coords$tintshade*2)-1)
                                ggname("geom_point_tintshade",
                                       pointsGrob(
                                         coords$x, coords$y,
                                         pch = coords$shape,
                                         gp = gg_par(
                                           col = alpha(coords$colour, coords$alpha),
                                           fill = fill_alpha(coords$fill, coords$alpha),
                                           pointsize = coords$size
                                         )
                                       )
                                )
                              },
                              draw_key = function(self, ...) draw_key_point_tintshade(...)
)
scale_tintshade_discrete <- function(name = waiver(), ..., range = c(0.2, 0.8)) {
  discrete_scale(
    aesthetics="tintshade", name = name, ...,
    palette = function(n) seq(range[1], range[2], length.out = n)
  )
}
draw_key_point_tintshade <- function(data, params, size){
  print(data)
  tintshade_colors <- lighten(data$colour, amount = (data$tintshade*2)-1)
  pointsGrob(0.5, 0.5, pch = 19, gp = gpar(
    col = alpha(tintshade_colors, data$alpha),
    fill = fill_alpha(data$fill %||% "black", data$alpha),
    fontsize = (data$size %||% 1.5) * .pt + 0.5 * .stroke/2,
    lwd = 0.5 * .stroke/2))
}
GuideTintshade <- ggproto(
  "Guide", GuideLegend,
  params = c(GuideLegend$params, list(col_tintshade = NULL))
  extract_key = function(scale, aesthetic, key, ...) {
    key$aesthetic <- scale$map(key$aesthetic)
    names(key)[names(key) == "aesthetic"] <- aesthetic
    key
  }
)

metab_data <- data.frame(
  metab=rep(c("Alanine", "Threonine", "Glycine", "GBT", "Proline betaine",
              "Carnitine", "DMSP", "DMS-Ac", "Isethionate"), 3),
  metab_group=rep(rep(c("Amino acid", "Betaine", "Sulfur"), each=3), 3),
  tripl=rep(c("A", "B", "C"), each=9),
  area=runif(27)
)
gp <- ggplot(metab_data) +
  geom_point_tintshade(aes(x = tripl, y = area, color = metab_group, tintshade = metab), size=4)

get_guide_data(plot = gp, "tintshade")
get_guide_data(plot = gp, "colour")

# ggplot(metab_data) +
#   geom_point_tintshade(aes(x = tripl, y = area, color = metab_group, tintshade = metab), size=4) +
#   guides(tintshade=guide_legend(override.aes = list(
#     color=c("#F8766D", "#00BA38", "#619CFF", "#619CFF", "#F8766D", "#00BA38", "#619CFF", "#00BA38", "#F8766D")
#   )))
