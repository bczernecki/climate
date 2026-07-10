context("hydro_imgw")

test_that("hydro_imgw_not_available", {
  
  expect_error(suppressWarnings(hydro_imgw(interval = "monthly", year = 1960, 
                          station = "not available", allow_failure = FALSE)))
  
  # monthly download for a single station:
  w = hydro_imgw(interval = "monthly", year = 2010, station = "WIGRY", 
                 allow_failure = FALSE)
  expect_true(is.data.frame(w) && nrow(w) == 36)
  
  h2022_2023 = hydro_imgw(interval = "monthly", 
                          year = 2022:2023, 
                          allow_failure = FALSE)
  
  if (!is.null(h2022_2023)) {
    if (is.data.frame(h2022_2023) & nrow(h2022_2023 > 50000)) {
      testthat::expect_true(is.data.frame(h2022_2023))
      testthat::expect_true(nrow(h2022_2023) > 50000)
      testthat::expect_true(class(h2022_2023$Data) == "Date")
    }
  }
  
  h2022_2023d = hydro_imgw(interval = "daily", 
                          year = 2022:2023, 
                          allow_failure = FALSE)
  if (!is.null(h2022_2023d)) {
    if (is.data.frame(h2022_2023d) & nrow(h2022_2023d > 50000)) {
      testthat::expect_true(is.data.frame(h2022_2023d))
      testthat::expect_true(nrow(h2022_2023d) > 50000)
      testthat::expect_true(class(h2022_2023d$Data) == "Date")
    }
  }
  
})