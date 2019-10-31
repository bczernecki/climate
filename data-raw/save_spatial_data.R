library(readr)

imgw_hydro_stations = read_csv("data-raw/hydro_stations.csv")
imgw_meteo_stations = read_csv("data-raw/meteo_stations.csv")

usethis::use_data(imgw_hydro_stations, overwrite = TRUE)
usethis::use_data(imgw_meteo_stations, overwrite = TRUE)
