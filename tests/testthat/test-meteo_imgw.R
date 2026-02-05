context("meteo_imgw")
y <-  2018

test_that("meteo_imgw works!", {
  
  if (!curl::has_internet()) {
    message("No internet connection! \n")
    return(invisible(NULL))
  } else {
    x <- meteo_imgw("hourly", "synop", year = y)
    x <- meteo_imgw("hourly", "climate", year = y)
    expect_message(x <- meteo_imgw("hourly", "precip", year = y))
    x <- meteo_imgw("daily", "synop", year = y)
    x <- meteo_imgw("daily", "climate", year = y)
    x <- meteo_imgw("daily", "precip", year = y)
    x <- meteo_imgw("monthly", "synop", year = y)
    x <- meteo_imgw("monthly", "climate", year = y)
    x <- meteo_imgw("monthly", "precip", year = y)
    x <- meteo_imgw("monthly", "synop", year = y, status = TRUE)
    x <- meteo_imgw("monthly", "synop", year = y, coords = TRUE)
    x <- meteo_imgw("monthly", "synop", year = y, col_names = "full")
    x <- meteo_imgw("monthly", "synop", year = y, coords = TRUE, col_names = "polish")
    
    testthat::expect_message(x <- suppressWarnings(meteo_imgw_daily(rank = "synop", year = 2001, station = "blabla")))
  }
})


test_that("meteo_imgw monthly works!", {
  
  if (!curl::has_internet()) {
    message("No internet connection! \n")
    return(invisible(NULL))
  } else {
    x = meteo_imgw(interval = "monthly", rank = "synop", year = 2020:2021, station = "BIAŁYSTOK")
    testthat::expect_equal(nrow(x), 24)
  }
})