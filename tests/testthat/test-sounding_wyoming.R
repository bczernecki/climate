
test_that("sounding_wyoming works!", {

  # expected error
  testthat::expect_message(sounding_wyoming(wmo_id = 12220,
                                            yy = 2019,
                                            mm = 4,
                                            dd = 4,
                                            hh = 0, allow_failure = FALSE))

  # expected error
  testthat::expect_error(sounding_wyoming(wmo_id = c(12220, 12375),
                                          yy = 2019,
                                          mm = 4,
                                          dd = 4,
                                          hh = 0, allow_failure = FALSE))
  
  # expected error for bufr
  testthat::expect_error(sounding_wyoming(wmo_id = 12375,
                                          yy = 2019,
                                          mm = 4,
                                          dd = 4,
                                          hh = 0,
                                          bufr = TRUE, allow_failure = FALSE))
})
