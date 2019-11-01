context("hydro-metadata")

h_d <- hydro_metadata_imgw("daily")
h_m <- hydro_metadata_imgw("monthly")
h_a <- hydro_metadata_imgw("semiannual_and_annual")

test_that("hydro-metadata works!", {
  expect_equal(dim(h_d[[1]]), c(10, 1))
  expect_equal(dim(h_d[[2]]), c(10, 1))
  expect_equal(dim(h_m[[1]]), c(10, 1))
  expect_equal(dim(h_a[[1]]), c(16, 1))
  expect_equal(dim(h_a[[2]]), c(16, 1))
  expect_equal(dim(h_a[[3]]), c(16, 1))
})
