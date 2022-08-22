test_that("nearest_stations_noaa", {
  x = nearest_stations_noaa(country = "SRI LANKA",
                            point = c(80, 6),
                            add_map = TRUE,
                            no_of_stations = 10)
  if (is.data.frame(x)) {
  testthat::expect_true(nrow(x) <= 10)
  }
  
})


test_that("nearest_stations_noaa_errors", {
  testthat::expect_message(nearest_stations_noaa(allow_failure = TRUE))
  testthat::expect_error(nearest_stations_noaa(allow_failure = FALSE))
  testthat::expect_error(nearest_stations_noaa(country = "POLAND",
                                               point = c(10, 20, 30),
                                               allow_failure = FALSE))
  
  testthat::expect_error(nearest_stations_noaa(country = "POLAND",
                                               point = 1,
                                               allow_failure = FALSE))
  
  # check if this is query is retrieving data for Poland:
  x = nearest_stations_noaa(country = "POLAND", point = c(19, 52))
  
  if (is.data.frame(x)) {
    testthat::expect_equal(unique(x$countries), "POLAND")
  }

  testthat::expect_error(nearest_stations_noaa(country = "POLAND",
                                               point = c(30, 50), 
                                               date = c(Sys.Date() - 7, Sys.Date() - 1), 
                                               allow_failure = FALSE))
  
  testthat::expect_error(nearest_stations_noaa(country = "SOVIET UNION",
                                               point = c(30, 50), 
                                               date = c(Sys.Date()),
                                               allow_failure = FALSE))

})
