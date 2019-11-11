library(remotes)
install_github("bczernecki/climate")
library(climate)
x <- meteo_ogimet(interval = "daily", date = c("2019-06-01", "2019-07-08"),
                        station = c(12330, 22222), coords = FALSE)
profile <- sounding_wyoming(wmo_id = 12220, yy = 2019, mm = 4, dd = 4, hh = 0)
