test_that("nearest_stations_noaa", {
  x = nearest_stations_nooa(country = "SRI LANKA",
                            point = c(80, 6),
                            add_map = TRUE,
                            no_of_stations = 10)
  testthat::expect_true(nrow(x) <= 10)
  
})
