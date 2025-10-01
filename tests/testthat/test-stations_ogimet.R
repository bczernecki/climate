test_that("stations_ogimet", {
  
  skip_on_cran()
  skip_if_offline()
  
  x = suppressWarnings(stations_ogimet(country = "Australia", add_map = TRUE))
  if (is.data.frame(x)) { 
    testthat::expect_true(nrow(x) >= 100)
    testthat::expect_equal(ncol(x), 5)
  }
  
  if (requireNamespace("maps", quietly = TRUE)) {
    suppressWarnings(stations_ogimet(country = "Australia", add_map = TRUE))
  }

  expect_error(stations_ogimet(country = NULL, add_map = FALSE, allow_failure = FALSE))
  expect_error(stations_ogimet(date = NULL, add_map = FALSE, allow_failure = FALSE))
})
