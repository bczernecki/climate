context("hydro_imgw")
y <- 2017

test_that("hydro_imgw_not_available", {
  
  expect_error(suppressWarnings(hydro_imgw(interval = "daily", year = 1960, coord = TRUE,
                    station = "not available", allow_failure = FALSE)))

  expect_error(suppressWarnings(hydro_imgw(interval = "daily", year = 1960, coord = TRUE,
                          station = 999, allow_failure = FALSE)))
  
  expect_error(suppressWarnings(hydro_imgw(interval = "monthly", year = 1960, coord = TRUE,
                          station = "not available", allow_failure = FALSE)))
  
  expect_error(suppressWarnings(hydro_imgw(interval = "monthly", year = 1960, coord = TRUE,
                          station = 999, allow_failure = FALSE)))
  
  expect_error(suppressWarnings(hydro_imgw(interval = "semiannual_and_annual", year = 1960, coord = TRUE,
                          station = "not available", allow_failure = FALSE)))
  
  expect_error(suppressWarnings(hydro_imgw(interval = "semiannual_and_annual", year = 1960, coord = TRUE,
                          station = 999, allow_failure = FALSE)))
})