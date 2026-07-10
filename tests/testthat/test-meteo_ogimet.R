
test_that("meteo_ogimet works!", {
  
  skip_on_cran()
  
  df = meteo_ogimet(interval = "daily", date = c("2019-06-01", "2019-06-08"),
                    station = c(12330, 12120))
  
  # sometimes ogimet requires warm spin-up, so in order to pass CRAN tests:
  if (is.data.frame(df) & nrow(df) > 15) {
    expect_true(any(colnames(df) %in% c("Lon", "Lat")))
  }
  
  # expected at least 100 rows in hourly HTML dataset (explicit source = "html"):
  Sys.sleep(20)
  x = meteo_ogimet(interval = "hourly", source = "html",
                   date = c("2019-06-01", "2019-06-08"),
                   station = c(12330))
  
  if (is.data.frame(x)) {
    testthat::expect_true(nrow(x) > 100)
  }
  
  # check if January is going to be downloaded (HTML path keeps $Date column):
  Sys.sleep(20)
  y = meteo_ogimet(interval = "hourly", source = "html",
                   date = c("2019-01-01", "2019-01-05"),
                   station = 12120)
  
  if (is.data.frame(y)) {
    testthat::expect_equal(unique(format(y$Date, "%Y")), "2019")
  }
  
  
  # check error for non existing station or problem with downloading any reasonable data:
  
  # wrong station ID (HTML path emits a warning):
  Sys.sleep(25)
  expect_warning(meteo_ogimet(interval = "hourly", source = "html",
                               date = c("2019-01-01", "2019-01-05"),
                               station = 999999, allow_failure = FALSE))
  # no date (HTML path):
  Sys.sleep(20)
  expect_message(meteo_ogimet(interval = "hourly", source = "html",
                               date = c(NA, NA),
                               station = 12120, allow_failure = TRUE))
  
  # no values for the selected station
  Sys.sleep(20)
  expect_message(meteo_ogimet(station = "64551", interval = "daily",
                              date = c("2025-09-26", "2025-09-26")))
  
  # not all elements available:
  # test temporarily turned off due to non-deterministic response from remote
  # Sys.sleep(25)
  # expect_equal(nrow(meteo_ogimet(station = "64556", interval = "daily",
  #                                date = c("2025-09-26", "2025-09-26"))), 1)
  # 
  # no interval provided: now defaults to "hourly", so no error expected
  # (test removed - interval has a default value of "hourly")
  
  # split works only for daily:
  Sys.sleep(20)
  expect_warning(meteo_ogimet(station = "06683",
                            date = c("2020-02-01", "2020-02-01"),
                            precip_split = FALSE, 
                            interval = "daily",
                            allow_failure = TRUE))
  
  
  # check precip_split on empty precipitation field
  Sys.sleep(20)
  petrobaltic = ogimet_hourly(station = 12001,
                        date = c(as.Date("2020-01-01"), as.Date("2020-01-05")),
                        precip_split = TRUE)
  if (is.data.frame(petrobaltic) & nrow(petrobaltic) > 0) {
   testthat::expect_true(all(is.na(petrobaltic$pr12)))
   Sys.sleep(20)
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
        station = "06683", allow_failure = FALSE)
    )
    
    # check dates between 2 years and check whether number of days is OK:
    Sys.sleep(20)
    multiyr = ogimet_daily(date = c(as.Date("2022-12-15"), as.Date("2023-01-21")), 
                           station = 12330)
    if (is.data.frame(multiyr)) {
      testthat::expect_true(nrow(multiyr) > 35)
    }
  }
})

# ── New unified interface tests ───────────────────────────────────────────────

test_that("meteo_ogimet hourly defaults to SYNOP and returns expected columns", {
  if (!curl::has_internet()) return(invisible(NULL))
  skip_on_cran()

  result = meteo_ogimet(interval = "hourly",
                        station  = 12330,
                        date     = c("2009-12-01", "2009-12-04"))
  if (is.null(result)) return(invisible(NULL))

  expect_s3_class(result, "data.frame")
  expect_true(all(c("date", "station", "t2m", "ws", "Nt") %in% names(result)))
  expect_s3_class(result$date, "POSIXct")
  expect_equal(attr(result$date, "tzone"), "UTC")
  expect_true(nrow(result) > 0)
})

test_that("meteo_ogimet hourly SYNOP date column is clipped to requested range", {
  if (!curl::has_internet()) return(invisible(NULL))
  skip_on_cran()

  result = meteo_ogimet(interval = "hourly",
                        station  = 12330,
                        date     = c("2009-12-01", "2009-12-04"))
  if (is.null(result) || nrow(result) == 0) return(invisible(NULL))

  expect_true(all(as.Date(result$date) >= as.Date("2009-12-01")))
  expect_true(all(as.Date(result$date) <= as.Date("2009-12-04")))
})


test_that("meteo_ogimet warns when precip_split = FALSE used with SYNOP", {
  expect_warning(
    suppressMessages(
      meteo_ogimet(interval = "hourly", station = 12330,
                   date = c("2009-12-01", "2009-12-01"),
                   precip_split = FALSE, allow_failure = TRUE)
    ),
    "precip_split"
  )
})

test_that("meteo_ogimet warns when return_list = TRUE used with source = 'html'", {
  expect_warning(
    suppressMessages(
      meteo_ogimet(interval = "daily", station = 12330,
                   date = c("2019-06-01", "2019-06-01"),
                   source = "html", return_list = TRUE, allow_failure = TRUE)
    ),
    "return_list"
  )
})

test_that("meteo_ogimet return_list = TRUE gives a named list with data and full", {
  if (!curl::has_internet()) return(invisible(NULL))
  skip_on_cran()

  result = meteo_ogimet(interval     = "hourly",
                        station      = 12330,
                        date         = c("2009-12-01", "2009-12-04"),
                        return_list  = TRUE)
  if (is.null(result)) return(invisible(NULL))

  expect_type(result, "list")
  expect_true(all(c("data", "full") %in% names(result)))
  expect_s3_class(result$data, "data.frame")
  expect_s3_class(result$full, "data.frame")
  expect_true(all(c("date", "t2m") %in% names(result$data)))
  expect_true("air_temperature" %in% names(result$full))
  expect_true(ncol(result$full) > ncol(result$data))
})

test_that("meteo_ogimet source = 'synop' works for daily interval", {
  if (!curl::has_internet()) return(invisible(NULL))
  skip_on_cran()

  result = meteo_ogimet(interval = "daily",
                        station  = 12330,
                        date     = c("2009-12-01", "2009-12-04"),
                        source   = "synop")
  if (is.null(result)) return(invisible(NULL))

  expect_s3_class(result, "data.frame")
  expect_true("date" %in% names(result))
  expect_true(nrow(result) > 0)
})
