test_that("meteo_noaa_hourly", {
  noaa = meteo_noaa_hourly(station = "123300-99999", year = 2019)
  
  if (is.data.frame(noaa)) {
    expect_true(all(noaa$year == 2019))
  }
  
  expect_message(meteo_noaa_hourly(station = "00000-99999", year = 2100))
})