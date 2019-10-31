context("meteo-abbrev")

test_that("meteo-metadata works!", {
  expect_equal(dim(imgw_meteo_abbrev), c(252, 3))
})
