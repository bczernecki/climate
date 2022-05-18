context("meteo_ogimet")
y <-  2018

test_that("meteo_ogimet works!", {
  x2 <- meteo_ogimet(interval = "hourly", date = c("2019-06-01", "2019-07-08"),
                     station = 12330, coords = TRUE, precip_split = TRUE)
  
  x <- meteo_ogimet(interval = "hourly", date = c("2019-06-01", "2019-07-08"),
                    station = 12330, coords = TRUE, precip_split = FALSE)
  
  meteo_ogimet(interval = "daily", date = c("2019-06-01", "2019-07-08"),
                    station = c(12330, 12375), coords = FALSE)
  
  # expected warning
  testthat::expect_warning(
    meteo_ogimet(interval = "daily", date = c("2019-06-01", "2019-07-08"),
                    station = c(12330, 22222), coords = FALSE)
  )
  
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



