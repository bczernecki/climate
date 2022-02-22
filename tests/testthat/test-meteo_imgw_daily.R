context("meteo_imgw_daily")
y <-  1900 # year not supported

test_that("meteo_imgw_daily", {
  expect_error(meteo_imgw_daily(rank = "synop", year = y, status = TRUE, coords = TRUE))
})
