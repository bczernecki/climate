context("meteo_imgw_daily")
y <-  1900 # year not supported

test_that("meteo_imgw_daily", {
  expect_error(meteo_imgw_daily(rank = "synop", year = y, status = TRUE, coords = TRUE))
})


test_that("check_column_with_coordinates", {
  station_with_coordinates = meteo_imgw_daily(rank = "precip",
                                              year = 2002,
                                              coords = TRUE,
                                              station = "IMBRAMOWICE")
  expect_true(any(colnames(station_with_coordinates) %in% c("X", "Y")))
})

test_that("check_error_for_non_existing_station", {
  expect_error(meteo_imgw_daily(rank = "precip",
                                year = 2002,
                                coords = TRUE,
                                station = 9999))
})
