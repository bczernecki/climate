test_that("compute_relative_humidity returns 100% when t2m equals dpt2m", {
  expect_equal(compute_relative_humidity(0, 0),    100)
  expect_equal(compute_relative_humidity(20, 20),  100)
  expect_equal(compute_relative_humidity(-10, -10), 100)
})

test_that("compute_relative_humidity returns plausible value for known inputs", {
  rh = compute_relative_humidity(t2m = 20, dpt2m = 10)
  expect_gt(rh, 50)
  expect_lt(rh, 55)
})

test_that("compute_relative_humidity is vectorised", {
  rh = compute_relative_humidity(t2m = c(20, 0), dpt2m = c(20, 0))
  expect_equal(rh, c(100, 100))
})

test_that("compute_relative_humidity propagates NA", {
  expect_true(is.na(compute_relative_humidity(NA_real_, 10)))
  expect_true(is.na(compute_relative_humidity(20, NA_real_)))
})

test_that("compute_relative_humidity stops on non-numeric input", {
  expect_error(compute_relative_humidity("20", 10))
  expect_error(compute_relative_humidity(20, "10"))
})

test_that("compute_relative_humidity stops on mismatched lengths", {
  expect_error(compute_relative_humidity(c(20, 15), 10))
})

test_that("compute_relative_humidity decreases as dpt2m decreases from t2m", {
  rh_high = compute_relative_humidity(20, 18)
  rh_low  = compute_relative_humidity(20, 5)
  expect_gt(rh_high, rh_low)
})
