
library(ggplot2)
library(colorspace)
library(grid)
# devtools::load_all()
metab_data <- data.frame(
  metab=rep(c("Alanine", "Threonine", "Glycine",
              "Glycine betaine", "Proline betaine", "Carnitine",
              "DMSP", "DMS-Ac", "Isethionate"), 3),
  metab_group=rep(rep(c("Amino acid", "Betaine", "Sulfur"), each=3), 3),
  tripl=rep(c("A", "B", "C"), each=9),
  area=runif(27)
)

ggplot(metab_data) +
  geom_point_tintshade(aes(x = tripl, y = area, color = metab_group, tintshade = metab), size=4)

ggplot(metab_data) +
  geom_point_tintshade(aes(x = tripl, y = area, color = metab_group, tintshade = metab), size=4) +
  guides(tintshade=guide_legend(override.aes = list(
    color=c("#F8766D", "#00BA38", "#619CFF", "#619CFF", "#F8766D", "#00BA38", "#619CFF", "#00BA38", "#F8766D")
  )))



#   scale_color_manual(breaks = c("Amino acid", "Betaine", "Sulfur"),
#                    values = c("#f4bb23", "#028e34", "#0b505c")) +
#   scale_tintshade_discrete(range = c(0.5, 0.8))
