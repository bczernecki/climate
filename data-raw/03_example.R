library(climate)
ns = nearest_stations_ogimet(point = c(-4, 56), no_of_stations = 50, add_map = TRUE)
head(ns)
#>    wmo_id       station_names       lon       lat alt  distance [km]
#> 29  03144         Strathallan  -3.733348 56.31667  35      46.44794
#> 32  03155           Drumalbin  -3.733348 55.61668 245      52.38975
#> 30  03148           Glen Ogle  -4.316673 56.41667 564      58.71862
#> 27  03134   Glasgow Bishopton  -4.533344 55.90002  59      60.88179
#> 35  03166 Edinburgh Gogarbank  -3.350007 55.93335  57      73.30942
#> 28  03136      Prestwick RNAS  -4.583345 55.51668  26      84.99537