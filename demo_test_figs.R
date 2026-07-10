
devtools::load_all()

metab_data <- data.frame(
  metab = rep(c("Alanine", "Threonine", "Glycine",
                "Glycine betaine", "Proline betaine", "Carnitine",
                "DMSP", "DMS-Ac", "Isethionate"), 3),
  metab_group = rep(rep(c("Amino acid", "Betaine", "Sulfur"), each = 3), 3),
  tripl = rep(c("A", "B", "C"), each = 9),
  area  = runif(27)
)

ggplot(metab_data) +
  geom_point_tintshade(aes(x = tripl, y = area, colour = metab_group, tintshade = metab), size = 4)

metab_data$metab <- factor(metab_data$metab, levels = unique(metab_data$metab))
ggplot(metab_data) +
  geom_point_tintshade(aes(x = tripl, y = area, colour = metab_group, tintshade = metab), size = 4)
ggplot(metab_data) +
  geom_point_tintshade(aes(x = tripl, y = area, fill = metab_group, tintshade = metab), size = 4, color="black", pch=21)

ggplot(metab_data) +
  geom_text_tintshade(aes(x = tripl, y = area, colour = metab_group, tintshade = metab, label=metab))

# Maybe? Nested.
ggplot(metab_data) +
  geom_col_tintshade(aes(x=tripl, y=area, fill=metab_group, tintshade = metab)) +
  ggtitle("Nested metabolite data")

ggplot(metab_data) +
  geom_boxplot_tintshade(aes(x=metab_group, y=area, color=metab_group, tintshade = metab))



align_data <- data.frame(
  alignment=1:9,
  moral=rep(c("good", "neutral", "evil"), each=3),
  meta=rep(c("lawful", "neutral", "chaotic"), length.out=9)
)
align_data$moral <- factor(align_data$moral, levels=rev(unique(align_data$moral)))
align_data$meta <- factor(align_data$meta, levels=unique(align_data$meta))
ggplot(align_data) +
  geom_point_tintshade(aes(x=moral, y=meta, color=meta, tintshade=moral), size=5)
# Maybe? Crossed.
ggplot(align_data) +
  geom_raster_tintshade(aes(x=meta, y=moral, fill=meta, tintshade=moral)) +
  scale_fill_manual(breaks = c("lawful", "neutral", "chaotic"), values=c("#cca40a", "#b52060", "#7623b2")) +
  coord_equal() +
  ggtitle("Crossed alignment chart")



ggplot(penguins) + geom_point_tintshade(aes(x=bill_len, y=bill_dep, fill=species, tintshade=sex), pch=21, color="black", size=3)

# Maybe? Crossed.
ggplot(diamonds) +
  geom_bar_tintshade(aes(x=cut, fill = cut, tintshade = clarity), color="black") +
  ggtitle("Crossed diamonds")

grp <- c(I1 = "I", SI2 = "SI", SI1 = "SI", VS2 = "VS", VS1 = "VS", VVS2 = "VVS", VVS1 = "VVS", IF = "IF")
diamonds$clarity_group <- factor(grp[as.character(diamonds$clarity)], levels = c("I", "SI", "VS", "VVS", "IF"))
mp <- aggregate(price ~ clarity + clarity_group, diamonds, mean)
# Maybe? Nested.
ggplot(mp) +
  geom_col_tintshade(aes(clarity, price, fill = clarity_group, tintshade = clarity)) +
  scale_tintshade_discrete(range = c(0.4, 0.6)) +
  ggtitle("Nested diamonds")


ggplot() + geom_point_tintshade(aes(x=1:10, y=1:10, tintshade=1:10), color="blue", size=5)

ggplot() + geom_point_tintshade(aes(x=1:10, y=1:10), color="red", tintshade=0.2, size=5)



mpgsub <- mpg[1:60,]
mpgsub$model <- factor(mpgsub$model, levels=unique(mpgsub$model))
# Maybe? Nested.
ggplot(mpgsub, aes(displ, hwy, colour = manufacturer, tintshade = model)) +
  geom_point_tintshade(size = 3) +
  ggtitle("Nested mpg")
