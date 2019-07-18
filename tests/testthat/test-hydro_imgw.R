context("hydro_imgw")
y <- 2017

test_that("hydro_imgw works!", {
  x <- hydro_imgw("daily", year = y)
  x <- hydro_imgw("monthly", year = y)
  x <- hydro_imgw("semiannual_and_annual", year = y, value = "H")
  x <- hydro_imgw("semiannual_and_annual", year = y, value = "Q")
  x <- hydro_imgw("semiannual_and_annual", year = y, value = "T")
  x <- hydro_imgw("semiannual_and_annual", year = y, coords = TRUE)
  x <- hydro_imgw("semiannual_and_annual", year = y, col_names = "full")
  x <- hydro_imgw("semiannual_and_annual", year = y, coords = TRUE, col_names = "full")
  x <- hydro_imgw("semiannual_and_annual", year = y, col_names = "polish")
  x <- hydro_imgw("semiannual_and_annual", year = y, coords = TRUE, col_names = "polish")
  x <- hydro_imgw("semiannual_and_annual", year = y, station = "BOGUSŁAW")
  x2 <- hydro_imgw("semiannual_and_annual", year = y, station = 149180020)
})

