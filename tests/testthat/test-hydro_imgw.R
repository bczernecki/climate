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
  
  h2022_2023 = hydro_imgw(interval = "monthly", 
                          year = 2022:2023, 
                          coord = TRUE,
                          allow_failure = FALSE)
  
  if (is.data.frame(h2022_2023) & nrow(h2022_2023 > 50000)) {
    testthat::expect_true(is.data.frame(h2022_2023))
    testthat::expect_true(nrow(h2022_2023) > 50000)
  }
  
  
  expect_error(suppressWarnings(hydro_imgw(interval = "semiannual_and_annual", year = 1960, coord = TRUE,
                          station = "not available", allow_failure = FALSE)))
  
  expect_error(suppressWarnings(hydro_imgw(interval = "semiannual_and_annual", year = 1960, coord = TRUE,
                          station = 999, allow_failure = FALSE)))
})