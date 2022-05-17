test_that("meteo_noaa_hourly", {
  co2 = meteo_noaa_co2()
  expect_true(is.data.frame(co2))
  expect_true(nrow(co2) > 700)
  expect_true(ncol(co2) >= 7)
})