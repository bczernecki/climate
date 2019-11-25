context("meteo_imgw")

test_that("nearest_stations_ogimet works!", {
  x <- nearest_stations_ogimet(country = "United+Kingdom", point = c(10, 50), add_map = FALSE, no_of_stations = 10)
  
  x <- nearest_stations_ogimet(country = "United+Kingdom", point = c(10, 50), add_map = TRUE, no_of_stations = 10)

  x <- nearest_stations_ogimet(country = "United+Kingdom", point = c(-10, -50), add_map = TRUE, no_of_stations = 10)
  
  x <- nearest_stations_ogimet(country = "Poland", point = c(10, 50), add_map = TRUE, no_of_stations = 10)
  
  # expected error
  x <- nearest_stations_ogimet(country = "Pland", point = c(10, 50), add_map = TRUE, no_of_stations = 10)
  
  x <- nearest_stations_ogimet(country = c("United+Kingdom", "Poland"), point = c(0, 0), add_map = TRUE, no_of_stations = 150)
  
})
