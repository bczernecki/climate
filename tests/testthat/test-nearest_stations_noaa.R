test_that("nearest_stations_noaa", {
  x = nearest_stations_nooa(country = "SRI LANKA",
                            point = c(80, 6),
                            add_map = TRUE,
                            no_of_stations = 10)
  testthat::expect_true(nrow(x) <= 10)
  
})


test_that("nearest_stations_noaa_errors", {
  testthat::expect_error(nearest_stations_nooa())
  testthat::expect_error(nearest_stations_nooa(country = "POLAND",
                                               point = c(10, 20, 30)))
  
  testthat::expect_error(nearest_stations_nooa(country = "POLAND",
                                               point = 1))
  
  testthat::expect_is(nearest_stations_nooa(country = "POLAND",
                                               point = NULL), "data.frame")
  
  testthat::expect_error(nearest_stations_nooa(country = "POLAND",
                                               point = c(30, 50), 
                                               date = c(Sys.Date() - 7, Sys.Date() - 1)))
  
  testthat::expect_error(nearest_stations_nooa(country = "SOVIET UNION",
                                               point = c(30, 50), 
                                               date = c(Sys.Date())))
  
  
})
