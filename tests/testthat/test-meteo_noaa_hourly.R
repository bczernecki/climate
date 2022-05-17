test_that("meteo_noaa_hourly", {
  noaa = meteo_noaa_hourly(station = "123300-99999", 
                           year = 2019)
  expect_true(all(noaa$year == 2019))
  
})