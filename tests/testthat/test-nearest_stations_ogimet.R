context("meteo_imgw")

test_that("nearest_stations_ogimet works!", {

  x <- nearest_stations_ogimet(country = "United+Kingdom", point = c(-10, -50), add_map = TRUE, no_of_stations = 10)
  
  if (is.data.frame(x)) {
    testthat::expect_equal(nrow(x), 10)
  }
  
  x <- nearest_stations_ogimet(country = "Poland", point = c(10, 50), add_map = TRUE, no_of_stations = 10)
  
  if (is.data.frame(x)) {
    testthat::expect_equal(nrow(x), 10)
  }
  
  # expected error
  # testthat::expect_message(nearest_stations_ogimet(country = "Pland",
  #                                                point = c(10, 50),
  #                                                add_map = TRUE,
  #                                                allow_failure = FALSE,
  #                                                no_of_stations = 10))
  
  x <- nearest_stations_ogimet(country = c("United+Kingdom", "Poland"), point = c(0, 0), add_map = TRUE, no_of_stations = 150)
  if (is.data.frame(x)) {
  expect_true(mean(x$distance) > 5000)
  }
  
})
