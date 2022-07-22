test_that("stations_ogimet", {
  x = suppressWarnings(stations_ogimet(country = "Australia", add_map = TRUE))

  testthat::expect_true(nrow(x) >= 100)
  testthat::expect_equal(ncol(x), 5)

  if (requireNamespace("maps", quietly = TRUE)) {
  x = suppressWarnings(stations_ogimet(country = "Australia", add_map = TRUE))
  }

  expect_error(stations_ogimet(country = NULL, add_map = FALSE))
  expect_error(stations_ogimet(date = NULL, add_map = FALSE))
})
