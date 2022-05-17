test_that("stations_ogimet", {
  x = suppressWarnings(stations_ogimet(country = "Australia", add_map = FALSE))
  
  testthat::expect_true(nrow(x) >= 100)
  testthat::expect_equal(ncol(x), 5)
  
})


