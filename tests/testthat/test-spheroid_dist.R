context("spheroid_dist")

test_that("spheroid_dist works!", {
  
  p1 = c(18.633333, 54.366667) # longitude and latitude for Gdansk
  p2 = c(17.016667, 54.466667) # longitude and latitude for Slupsk
  res = spheroid_dist(p1, p2)
  
  testthat::expect_true(res > 105 & res < 106)
  
})

