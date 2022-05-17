test_that("nearest_stations_imgw", {
  x = nearest_stations_imgw(
    type = "meteo",
    rank = "synop",
    year = 2018,
    add_map = TRUE,
    point = NULL,
    no_of_stations = 50
  )
  testthat::expect_true(nrow(x) <= 50)
  
})
