
library(ggplot2)
devtools::load_all()

metab_data <- data.frame(
  metab = rep(c("Alanine", "Threonine", "Glycine",
                "Glycine betaine", "Proline betaine", "Carnitine",
                "DMSP", "DMS-Ac", "Isethionate"), 3),
  metab_group = rep(rep(c("Amino acid", "Betaine", "Sulfur"), each = 3), 3),
  tripl = rep(c("A", "B", "C"), each = 9),
  area  = runif(27)
)

ggplot(metab_data, ) +
  geom_point_tintshade(aes(x = tripl, y = area, colour = metab_group, tintshade = metab), size = 4)

metab_data$metab <- factor(metab_data$metab, levels = unique(metab_data$metab))
ggplot(metab_data) +
  geom_point_tintshade(aes(x = tripl, y = area, colour = metab_group, tintshade = metab), size = 4)

ggplot(metab_data) +
  geom_text_tintshade(aes(x = tripl, y = area, colour = metab_group, tintshade = metab, label=metab))

ggplot(metab_data) +
  geom_col_tintshade(aes(x=tripl, y=area, fill=metab_group, tintshade = metab))

ggplot(metab_data) +
  geom_boxplot_tintshade(aes(x=metab_group, y=area, color=metab_group, tintshade = metab))



align_data <- data.frame(
  alignment=1:9,
  moral=rep(c("good", "neutral", "evil"), each=3),
  meta=rep(c("lawful", "neutral", "chaotic"), length.out=9)
)
align_data$moral <- factor(align_data$moral, levels=unique(align_data$moral))
align_data$meta <- factor(align_data$meta, levels=unique(align_data$meta))
ggplot(align_data) +
  geom_point_tintshade(aes(x=moral, y=meta, color=moral, tintshade=meta))
