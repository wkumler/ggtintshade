library(ggplot2)
library(ggtintshade)
library(patchwork)
library(ragg)

diamonds <- ggplot2::diamonds

levels(diamonds$cut) <- c("Fair", "Good", "Very\nGood", "Premium", "Ideal")

crossed <- ggplot(diamonds) +
  geom_bar_tintshade(aes(x = cut, fill = cut, tintshade = clarity), color = "black") +
  scale_y_continuous(expand = expansion(c(0, 0.05)), labels = scales::label_comma()) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    plot.margin = margin(0,0,0,0, "pt"),
    legend.box.margin = margin(0, 0, 0, 0, "pt"),
    legend.margin = margin(0, 0, 0, 0, "pt"),
    plot.title.position = "plot"
  ) +
  ggtitle("Diamond counts")

grp <- c(I1 = "I", SI2 = "SI", SI1 = "SI", VS2 = "VS", VS1 = "VS", VVS2 = "VVS", VVS1 = "VVS", IF = "IF")
diamonds$cl_grp <- factor(grp[as.character(diamonds$clarity)], levels = c("I", "SI", "VS", "VVS", "IF"))
mp <- aggregate(price ~ clarity + cl_grp, diamonds, median)

nested <- ggplot(mp) +
  geom_col_tintshade(aes(clarity, price, fill = cl_grp, tintshade = clarity), color = "black") +
  scale_y_continuous(expand = expansion(c(0, 0.05)), labels = scales::label_currency(scale_cut=scales::cut_short_scale())) +
  scale_tintshade_discrete(range = c(0.4, 0.6)) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    plot.margin = margin(0,0,0,0, "pt"),
    legend.box.margin = margin(0, 0, 0, 0, "pt"),
    legend.margin = margin(0, 0, 0, 0, "pt"),
    plot.title.position = "plot"
  ) +
  ggtitle("Median price")

divider <- wrap_elements(
  panel = grid::linesGrob(
    x = unit(c(0.5, 0.5), "npc"),
    y = unit(c(0.05, 0.95), "npc"),
    gp = grid::gpar(col = "gray80", lwd = 1)
  )
)

total_gp <- crossed + divider+ nested +
  plot_layout(widths = c(0.65, 0.01, 0.35))
print(total_gp)

agg_png("gallery_fig.png", width = 700, height = 500, units = "px", res = 120)
print(total_gp)
dev.off()
