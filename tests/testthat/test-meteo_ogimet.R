
test_that("meteo_ogimet works!", {
  
  skip_on_cran()
  
  df = meteo_ogimet(interval = "daily", date = c("2019-06-01", "2019-06-08"),
                    station = c(12330, 12375), coords = TRUE)
  
  # sometimes ogimet requires warm spin-up, so in order to pass CRAN tests:
  if (is.data.frame(df) & nrow(df) > 15) {
    expect_true(any(colnames(df) %in% c("Lon", "Lat")))
  }
  
  # expected at least 100 rows in hourly dataset:
  x = meteo_ogimet(interval = "hourly", date = c("2019-06-01", "2019-06-08"),
               station = c(12330), coords = TRUE)
  
  if (is.data.frame(x) & nrow(df) > 20) {
    testthat::expect_true(nrow(x) > 100)
  }
  
  # check if January is going to be downloaded not other dates are downloaded by accident:
  x = meteo_ogimet(interval = "hourly", date = c("2019-01-01", "2019-01-05"),
               station = 12120, coords = FALSE)
  
  if (is.data.frame(x) & nrow(x) > 20) {
    testthat::expect_equal(unique(format(x$Date, "%Y")), "2019")
  }
  
  
  # check error for non existing station or problem with downloading any reasonable data:
  
  # wrong station ID:
  expect_warning(meteo_ogimet(interval = "hourly", date = c("2019-01-01", "2019-01-05"),
                       station = 999999, coords = FALSE, allow_failure = FALSE))
  # no date:
  expect_message(meteo_ogimet(interval = "hourly", 
                           date = c(NA, NA),
                           station = 12120, coords = FALSE, allow_failure = TRUE))
  
  # no values for the selected station
  expect_message(meteo_ogimet(station = "64551", interval = "daily",
                              date = c("2025-09-26", "2025-09-26")))
  
  # not all elements available:
  expect_equal(nrow(meteo_ogimet(station = "64556", interval = "daily",
               date = c("2025-09-26", "2025-09-26"))), 1)
  
  # no interval provided:
  expect_error(meteo_ogimet(station = "06683",
                            date = c("2020-02-01", "2020-02-01"),
                            coords = FALSE, allow_failure = TRUE))
  # split works only for daily:
  expect_warning(meteo_ogimet(station = "06683",
                            date = c("2020-02-01", "2020-02-01"),
                            precip_split = FALSE, 
                            interval = "daily",
                            coords = FALSE, allow_failure = TRUE))
  
  
  if (is.data.frame(x) & nrow(x) > 20) {
    testthat::expect_equal(unique(format(x$Date, "%Y")), "2019")
  }
  
  # check precip_split on empty precipitation field
  petrobaltic = ogimet_hourly(station = 12001,
                         date = c(as.Date("2020-01-01"), as.Date("2020-01-05")),
                         coords = TRUE, precip_split = TRUE)
  if (is.data.frame(petrobaltic) & nrow(petrobaltic) > 0) {
    testthat::expect_true(all(is.na(petrobaltic$pr12)))
  }

    
  # only wind measurement are present:
  if (!curl::has_internet()) {
    message("No internet connection! \n")
    return(invisible(NULL))
  } else {
    testthat::expect_message(
      meteo_ogimet(
        date = c(as.Date("2020-02-01"), Sys.Date() - 1),
        # date = c(Sys.Date() - 7, Sys.Date() - 1),
        interval = "daily",
        coords = FALSE,
        station = "06683", allow_failure = FALSE)
    )
    
    # check change between years:
    multiyr = ogimet_daily(date = c(as.Date("2022-12-15"), as.Date("2023-01-21")), 
                           station = 12330)
    if (is.data.frame(multiyr)) {
      testthat::expect_true(nrow(multiyr) > 35)
    }
  }
})
