library(readr)

hydro_stations = read_csv("data-raw/hydro_stations.csv")
meteo_stations = read_csv("data-raw/meteo_stations.csv")

usethis::use_data(hydro_stations, overwrite = TRUE)
usethis::use_data(meteo_stations, overwrite = TRUE)
