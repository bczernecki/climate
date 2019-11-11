context("meteo_ogimet")
y <-  2018

test_that("meteo_ogimet works!", {
  x2 <- meteo_ogimet(interval = "hourly", date = c("2019-06-01", "2019-07-08"),
                     station = 12330, coords = TRUE, precip_split = TRUE)
  
  x <- meteo_ogimet(interval = "hourly", date = c("2019-06-01", "2019-07-08"),
                    station = 12330, coords = TRUE, precip_split = FALSE)
  
  x <- meteo_ogimet(interval = "daily", date = c("2019-06-01", "2019-07-08"),
                    station = 12330, coords = TRUE)
  
  x <- meteo_ogimet(interval = "daily", date = c("2019-06-01", "2019-07-08"),
                    station = 12375, coords = FALSE)
  
  x <- meteo_ogimet(interval = "daily", date = c("2019-06-01", "2019-07-08"),
                    station = c(12330, 12375), coords = FALSE)
  
  # expected warning
  x <- meteo_ogimet(interval = "daily", date = c("2019-06-01", "2019-07-08"),
                    station = 22222, coords = FALSE)
  # expected warning
  x <- meteo_ogimet(interval = "daily", date = c("2019-06-01", "2019-07-08"),
                    station = c(12330, 22222), coords = FALSE)
})



