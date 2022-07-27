test_that("nearest_stations_imgw", {
  x = suppressWarnings(nearest_stations_imgw(
    type = "meteo",
    rank = "synop",
    year = 2018,
    add_map = TRUE,
    point = NULL,
    no_of_stations = 50
  )) 
  # added suppresswarnings as encoding may give extra warnings:
  testthat::expect_true(nrow(x) <= 50)

  # too many values provided for points  
   testthat::expect_error(suppressWarnings(nearest_stations_imgw(
     type = "meteo",
     rank = "synop",
     year = 2010,
     point = c(15, 50, 100),
     add_map = FALSE
   )
   )
   )
  
  # too high values in lon/lat:
  testthat::expect_error(nearest_stations_imgw(
    type = "meteo",
    rank = "synop",
    year = 2010,
    point = c(999, 999),
    add_map = FALSE
  )
  )
  
  testthat::expect_error(nearest_stations_imgw(
    type = "hydro",
    year = 2099:2100,
    point = c(0, 50),
    add_map = FALSE
  ))
  
  
})
