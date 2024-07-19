
# https://ggplot2.tidyverse.org/articles/extending-ggplot2.html#creating-new-guides

GuideKey <- ggproto(
  "Guide", GuideAxis,

  # Some parameters are required, so it is easiest to copy the base Guide's
  # parameters into our new parameters.
  # We add a new 'key' parameter for our own guide.
  params = c(GuideAxis$params, list(key = NULL)),

  # It is important for guides to have a mapped aesthetic with the correct name
  extract_key = function(scale, aesthetic, key, ...) {
    key$aesthetic <- scale$map(key$aesthetic)
    names(key)[names(key) == "aesthetic"] <- aesthetic
    print(head(key))
    key
  }
)
guide_key <- function(
    aesthetic, value = aesthetic, label = as.character(aesthetic),
    ...,
    # Standard guide arguments
    theme = NULL, title = waiver(), order = 0, position = waiver()
) {

  key <- data.frame(aesthetic, .value = value, .label = label, ...)

  new_guide(
    # Arguments passed on to the GuideKey$params field
    key = key, theme = theme, title = title, order = order, position = position,
    # Declare which aesthetics are supported
    available_aes = c("x", "y"),
    # Set the guide class
    super = GuideKey
  )
}
ggplot(mpg, aes(cty, hwy, colour = displ)) +
  geom_point() +
  scale_x_continuous(
    guide = guide_key(aesthetic = seq(10, 35, 5))
  )
