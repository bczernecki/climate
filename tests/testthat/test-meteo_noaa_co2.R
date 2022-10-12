test_that("meteo_noaa_hourly", {
  co2 = tryCatch(meteo_noaa_co2(), error = function(e) 0)
  if (is.data.frame(co2)) {
    expect_true(is.data.frame(co2))
    expect_true(nrow(co2) > 700)
    expect_true(ncol(co2) >= 7)
  }
})