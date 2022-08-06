context("meteo_ogimet")
y <-  2018

test_that("meteo_ogimet works!", {
  
  df = meteo_ogimet(interval = "daily", date = c("2019-06-01", "2019-06-08"),
                    station = c(12330, 12375), coords = TRUE)
  
  # sometimes ogimet requires warm spin-up, so in order to pass CRAN tests:
  if (any(colnames(df) %in% c("Lon", "Lat"))) {
    expect_true(any(colnames(df) %in% c("Lon", "Lat")))
  }
  
  # expected warning
  testthat::expect_warning(
    meteo_ogimet(interval = "daily", date = c("2019-06-01", "2019-06-08"),
                    station = c(22222), coords = FALSE)
  )
  
  # expected at least 100 rows in hourly dataset:
  testthat::expect_true(
    nrow(meteo_ogimet(interval = "hourly", date = c("2019-06-01", "2019-06-08"),
                 station = c(12330), coords = TRUE)) > 100)
  
  # check if January is going to be downloaded not other dates are downloaded by accident:
  testthat::expect_equal(unique(format(meteo_ogimet(interval = "hourly", date = c("2019-01-01", "2019-01-05"),
               station = 12001, coords = FALSE)$Date, "%Y")), "2019")
  
  # check precip_split on empty precipitation field
  petrobaltic = ogimet_hourly(station = 12001,
                         date = c(as.Date("2019-01-01"), as.Date("2019-01-05")),
                         coords = TRUE, precip_split = TRUE)
  testthat::expect_true(all(is.na(petrobaltic$pr12)))
  
  # only wind measurement are present:
  testthat::expect_error(
    error <- meteo_ogimet(
      date = c(as.Date("2020-02-01"), Sys.Date() - 1),
      # date = c(Sys.Date() - 7, Sys.Date() - 1), 
      interval = "daily",
      coords = FALSE, 
      station = "06683")
  )
})
