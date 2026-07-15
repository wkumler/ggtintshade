
test_that("factory makes new geom", {
  GeomPointTintshade <- make_tintshade_geom(ggplot2::GeomPoint)
  expect_identical(class(GeomPointTintshade), c("GeomPointTintshade", "GeomPoint", "Geom", "ggproto", "gg"))
  expect_identical(GeomPointTintshade$tintshade_base, ggplot2::GeomPoint)
  expect_contains(names(GeomPointTintshade), "tintshade_cache")
  expect_contains(names(GeomPointTintshade), "use_defaults")
  expect_contains(names(GeomPointTintshade), "default_aes")
})

test_that("constructor works correctly", {
  geom_point_tintshade <- make_geom_constructor(GeomPointTintshade)
  expect_contains(formals("geom_point_tintshade"), "mapping")
  expect_contains(formals("geom_point_tintshade"), "data")
  expect_identical(formals("geom_point_tintshade")$stat, "identity")
  expect_identical(formals("geom_point_tintshade")$position, "identity")
})
