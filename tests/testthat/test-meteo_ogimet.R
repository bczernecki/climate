
test_that("meteo_ogimet works!", {
  
  df = meteo_ogimet(interval = "daily", date = c("2019-06-01", "2019-06-08"),
                    station = c(12330, 12375), coords = TRUE)
  
  # sometimes ogimet requires warm spin-up, so in order to pass CRAN tests:
  if (is.data.frame(df) & nrow(df) > 0) {
    expect_true(any(colnames(df) %in% c("Lon", "Lat")))
  }
  
  # expected warning
  # testthat::expect_message(
  #   meteo_ogimet(interval = "daily", date = c("2019-06-01", "2019-06-08"),
  #                   station = c(22222), coords = FALSE, allow_failure = TRUE)
  # )
  
  # expected at least 100 rows in hourly dataset:
  x = meteo_ogimet(interval = "hourly", date = c("2019-06-01", "2019-06-08"),
               station = c(12330), coords = TRUE)
  
  if (is.data.frame(x) & nrow(df) > 0) {
    testthat::expect_true(nrow(x) > 100)
  }
  
  # check if January is going to be downloaded not other dates are downloaded by accident:
  x = meteo_ogimet(interval = "hourly", date = c("2019-01-01", "2019-01-05"),
               station = 12001, coords = FALSE)
  
  if (is.data.frame(x) & nrow(df) > 0) {
    testthat::expect_equal(unique(format(x$Date, "%Y")), "2019")
  }
  
  # check precip_split on empty precipitation field
  petrobaltic = ogimet_hourly(station = 12001,
                         date = c(as.Date("2019-01-01"), as.Date("2019-01-05")),
                         coords = TRUE, precip_split = TRUE)
  if (is.data.frame(petrobaltic) & nrow(df) > 0) {
    testthat::expect_true(all(is.na(petrobaltic$pr12)))
  }

    
  # only wind measurement are present:
  if (!curl::has_internet()) {
    message("No internet connection! \n")
    return(invisible(NULL))
  } else {
    testthat::expect_error(
      meteo_ogimet(
        date = c(as.Date("2020-02-01"), Sys.Date() - 1),
        # date = c(Sys.Date() - 7, Sys.Date() - 1),
        interval = "daily",
        coords = FALSE,
        station = "06683", allow_failure = FALSE)
    )
    
    # check change between years:
    multiyr = ogimet_daily(date = c( as.Date("2022-11-15"),
                                     as.Date("2023-02-01")), 
                           station = 12330)
    if (is.data.frame(multiyr)) {
      testthat::expect_true(nrow(multiyr) > 70)
    }
  }
})
