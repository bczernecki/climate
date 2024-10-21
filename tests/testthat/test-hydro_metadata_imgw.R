context("hydro-metadata")

h_d <- suppressWarnings(hydro_metadata_imgw("daily"))
h_m <- suppressWarnings(hydro_metadata_imgw("monthly"))

test_that("hydro-metadata works!", {
  if (is.list(h_d) && is.list(h_m) && is.list(h_a)) {
    expect_equal(dim(h_d[[1]]), c(10, 1))
    expect_equal(dim(h_d[[2]]), c(10, 1))
    expect_equal(dim(h_m[[1]]), c(10, 1))
  }
})
