# climate <img src="man/figures/logo.png" align="right" width="150" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/bczernecki/climate/workflows/R-CMD-check/badge.svg)](https://github.com/bczernecki/climate/actions)
[![HTML5 check](https://github.com/bczernecki/climate/actions/workflows/html5-check.yaml/badge.svg?branch=master)](https://github.com/bczernecki/climate/actions/workflows/html5-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/bczernecki/climate/branch/master/graph/badge.svg)](https://app.codecov.io/gh/bczernecki/climate?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/climate)](https://cran.r-project.org/package=climate)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/climate)](https://cran.r-project.org/package=climate)
[![](https://cranlogs.r-pkg.org/badges/grand-total/climate?color=brightgreen)](https://cran.r-project.org/package=climate)
<!-- badges: end -->

The goal of the  **climate** R package is to automatize downloading of *in-situ* meteorological
and hydrological data from publicly available repositories:

- OGIMET [(ogimet.com)](http://ogimet.com/index.phtml.en) - up-to-date collection of SYNOP dataset
- University of Wyoming - atmospheric vertical profiling data (http://weather.uwyo.edu/upperair/)
- National Oceanic & Atmospheric Administration - Earth System Research Laboratories - Global Monitoring Laboratory [(NOAA)](https://gml.noaa.gov/ccgg/trends/)
- Polish Institute of Meteorology and Water Management - National Research Institute [(IMGW-PIB)](https://dane.imgw.pl/)
- National Oceanic & Atmospheric Administration - National Climatic Data Center - Integrated Surface Hourly (ISH) [(NOAA)](https://www1.ncdc.noaa.gov/pub/data/noaa/)

## Installation

The stable release of the **climate** package from the [CRAN](https://CRAN.R-project.org) repository can be installed with:

``` r
install.packages("climate")
```

It is highly recommended to install the most up-to-date development version of **climate** from [GitHub](https://github.com/bczernecki/climate) with:

``` r
library(remotes)
install_github("bczernecki/climate")
```

## Overview

### Meteorological data

- **meteo_ogimet()** - Downloading hourly and daily meteorological data from the SYNOP stations available in the ogimet.com collection.
Any meteorological (aka SYNOP) station working under the World Meteorological Organizaton framework after year 2000 should be accessible.

- **meteo_imgw()** - Downloading hourly, daily, and monthly meteorological data from the SYNOP/CLIMATE/PRECIP stations available in the danepubliczne.imgw.pl collection. 
It is a wrapper for `meteo_monthly()`, `meteo_daily()`, and `meteo_hourly()`. If 10-min dataset is needed then consider using **`meteo_imgw_datastore()`**

- **meteo_noaa_hourly()** - Downloading hourly NOAA Integrated Surface Hourly (ISH) meteorological data - Some stations have > 100 years long history of observations

- **sounding_wyoming()** - Downloading measurements of the vertical profile of atmosphere (aka rawinsonde data)

- **meteo_noaa_co2()** - Downloading monthly CO2 measurements from Mauna Loa Observatory

  
### Hydrological data

- **hydro_imgw()** - Downloading hourly, daily, and monthly hydrological data from stations available in the
danepubliczne.imgw.pl collection.
It is a wrapper for previously developed set of functions such as: `hydro_monthly()`, and `hydro_daily()`
- **hydro_imgw_datastore()** - Downloading hourly and subhourly hydrological data from the IMGW-PIB hydro telemetry stations.


### Auxiliary functions and datasets

- **stations_ogimet()** - Downloading information about all stations available in the selected
country in the Ogimet repository
- **nearest_stations_ogimet()** - Downloading information about nearest stations to the selected point using Ogimet repository
- **nearest_stations_noaa()** - Downloading information about nearest stations to the selected point available for the selected country in the NOAA ISH meteorological repository
- **nearest_stations_imgw()** - List of nearby meteorological or hydrological IMGW-PIB stations in Poland
- **imgw_meteo_stations** - Built-in   metadata from the IMGW-PIB repository for meteorological stations, their geographical coordinates, and ID numbers
- **imgw_hydro_stations** - Built-in metadata from the IMGW-PIB repository for hydrological stations, their geographical coordinates, and ID numbers
- **stations_meteo_imgw_telemetry** - Downloading complete and up-to-date information about coordinates for IMGW-PIB telemetry meteorological stations
- **stations_hydro_imgw_telemetry** - Downloading complete and up-to-date information about coordinates for IMGW-PIB telemetry hydrological stations
- **imgw_meteo_abbrev** - Dictionary explaining variables available for meteorological stations (from the IMGW-PIB repository)
- **imgw_hydro_abbrev** - Dictionary explaining variables available for hydrological stations (from the IMGW-PIB repository)

## Example 1
#### Download hourly dataset from NOAA ISH meteorological repository:

``` r0
library(climate)
noaa <- meteo_noaa_hourly(station = "123300-99999", year = 2018:2019) # station ID: Poznan, Poland
head(noaa)

#   year month day hour   lon    lat alt t2m dpt2m ws  wd    slp visibility
#   2019     1   1    0 16.85 52.417  84 3.3   2.3  5 220 1025.0       6000
#   2019     1   1    1 16.85 52.417  84 3.7   3.0  4 220 1024.2       1500
#   2019     1   1    2 16.85 52.417  84 4.2   3.6  4 220 1022.5       1300
#   2019     1   1    3 16.85 52.417  84 5.2   4.6  5 240 1021.2       1900
```

## Example 2
#### Finding a nearest meteorological stations in a given country using NOAA ISH data source:

``` r1
library(climate)
# find 100 nearest UK stations to longitude 1W and latitude 53N :

nearest_stations_ogimet(country = "United+Kingdom",
  date = Sys.Date(),
  add_map = TRUE,
  point = c(-1, 53),
  no_of_stations = 100
)

#  wmo_id                   station_names       lon      lat alt distance [km]
#    03354      Nottingham Weather Centre  -1.250005 53.00000 117      28.04973
#    03379                       Cranwell  -0.500010 53.03333  67      56.22175
#    03377                     Waddington  -0.516677 53.16667  68      57.36093
#    03373                       Scampton  -0.550011 53.30001  57      60.67897
#    03462                      Wittering  -0.466676 52.61668  84      73.68934
#    03544                 Church Lawford  -1.333340 52.36667 107      80.29844
# ...
```

![100 nearest stations to given coordinates in UK](http://iqdata.eu/kolokwium/uk.png)

## Example 3
#### Downloading daily (or hourly) data from a global (OGIMET) repository knowing its ID (see also `nearest_stations_ogimet()`):

``` r
library(climate)
o = meteo_ogimet(date = c(Sys.Date() - 5, Sys.Date() - 1), 
                 interval = "daily",
                 coords = FALSE, 
                 station = 12330)
head(o)

#>   station_ID       Date TemperatureCAvg TemperatureCMax TemperatureCMin TdAvgC HrAvg WindkmhDir
#> 3      12330 2019-12-21             8.8            13.2             4.9    5.3  79.3        SSE
#> 4      12330 2019-12-20             5.4             8.5            -1.2    4.5  92.4        ESE
#> 5      12330 2019-12-19             3.8            10.3            -3.0    1.9  89.6         SW
#> 6      12330 2019-12-18             6.3             9.0             2.2    4.1  84.8          S
#> 7      12330 2019-12-17             4.9             7.6             0.3    2.9  87.2        SSE
#>   WindkmhInt WindkmhGust PresslevHp Precmm TotClOct lowClOct SunD1h VisKm SnowDepcm PreselevHp
#> 3       11.4        39.6      995.9    1.8      3.6      2.0    6.7  21.4      <NA>         NA
#> 4       15.0          NA     1015.0    0.0      6.4      0.6    1.0   8.0      <NA>         NA
#> 5        7.1          NA     1020.4    0.0      5.2      5.9    2.5  14.1      <NA>         NA
#> 6        9.2          NA     1009.2    0.0      5.7      2.7    1.4  12.2      <NA>         NA
#> 7        7.2          NA     1010.8    0.1      6.2      4.6   <NA>  13.0      <NA>         NA
```

## Example 4
#### Downloading monthly/daily/hourly meteorological/hydrological data from the Polish (IMGW-PIB) repository:

``` r3
m = meteo_imgw(interval = "monthly", rank = "synop", year = 2000, coords = TRUE)
head(m)
#>            rank        id        X        Y   station   yy mm tmax_abs
#> 575 SYNOPTYCZNA 353230295 23.16228 53.10726 BIAŁYSTOK 2000  1      5.3
#> 577 SYNOPTYCZNA 353230295 23.16228 53.10726 BIAŁYSTOK 2000  2     10.6
#> 578 SYNOPTYCZNA 353230295 23.16228 53.10726 BIAŁYSTOK 2000  3     14.8
#> 579 SYNOPTYCZNA 353230295 23.16228 53.10726 BIAŁYSTOK 2000  4     27.8
#> 580 SYNOPTYCZNA 353230295 23.16228 53.10726 BIAŁYSTOK 2000  5     29.3
#> 581 SYNOPTYCZNA 353230295 23.16228 53.10726 BIAŁYSTOK 2000  6     32.6
#>     tmax_mean tmin_abs tmin_mean t2m_mean_mon t5cm_min rr_monthly
#> 575       0.4    -16.5      -4.5         -2.1    -23.5       34.2
#> 577       4.1    -10.4      -1.4          1.3    -12.9       25.4
#> 578       6.2     -6.4      -1.0          2.4     -9.4       45.5
#> 579      17.9     -4.6       4.7         11.5     -8.1       31.6
#> 580      21.3     -4.3       5.7         13.8     -8.3        9.4
#> 581      23.1      1.0       9.6         16.6     -1.8       36.4

h = hydro_imgw(interval = "daily", year = 2010:2011)
head(h)
          id station riv_or_lake       date  hyy idhyy dd   H   Q  T mm thick
1  150210180 ANNOPOL   Wisła (2) 2009-11-01 2010     1  1 287 436 NA 11    NA
2  150210180 ANNOPOL   Wisła (2) 2009-11-02 2010     1  2 282 412 NA 11    NA
3  150210180 ANNOPOL   Wisła (2) 2009-11-03 2010     1  3 272 368 NA 11    NA
4  150210180 ANNOPOL   Wisła (2) 2009-11-04 2010     1  4 268 352 NA 11    NA
5  150210180 ANNOPOL   Wisła (2) 2009-11-05 2010     1  5 264 336 NA 11    NA
6  150210180 ANNOPOL   Wisła (2) 2009-11-06 2010     1  6 260 320 NA 11    NA
```

## Example 5
#### Create Walter & Lieth climatic diagram based on downloaded data

``` r4
library(climate)
library(dplyr)

df = meteo_imgw(interval = "monthly", rank = "synop", year = 1991:2019, station = "POZNAŃ") 
df2 = select(df, station:t2m_mean_mon, rr_monthly)

monthly_summary = df2 %>% 
  group_by(mm) %>% 
  summarise(tmax = mean(tmax_abs, na.rm = TRUE), 
            tmin = mean(tmin_abs, na.rm = TRUE),
            tavg = mean(t2m_mean_mon, na.rm = TRUE), 
            prec = sum(rr_monthly) / n_distinct(yy))            

monthly_summary = as.data.frame(t(monthly_summary[, c(5,2,3,4)])) 
monthly_summary = round(monthly_summary, 1)
colnames(monthly_summary) = month.abb
print(monthly_summary)

#        Jan   Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov   Dec
# prec  37.1  31.3 38.5 31.3 53.9 60.8 94.8 59.6 40.5 39.7 35.7  38.6
# tmax   8.7  11.2 17.2 23.8 28.3 31.6 32.3 31.8 26.9 21.3 14.3   9.8
# tmin -15.0 -11.9 -7.6 -3.3  1.0  5.8  8.9  7.5  2.7 -2.4 -5.2 -10.4
# tavg  -1.0   0.5  3.7  9.4 14.4 17.4 19.4 19.0 14.3  9.1  4.5   0.8

# create plot with use of the "climatol" package:
climatol::diagwl(monthly_summary, mlab = "en", 
                 est = "POZNAŃ", alt = NA, 
                 per = "1991-2019", p3line = FALSE)
```

![Walter and Lieth climatic diagram for Poznan, Poland](http://iqdata.eu/kolokwium/poznan.svg)

## Example 6
#### Download monthly CO2 dataset from Mauna Loa observatory

``` r5
library(climate)
library(ggplot2)
library(ggthemes)

co2 = meteo_noaa_co2()
head(co2)
co2$date = ISOdate(co2$yy, co2$mm, 1)
ggplot(co2, aes(date, co2_avg)) + 
  geom_line()+ geom_smooth()+
  theme_bw()+
  labs(
    title = "Carbon Dioxide (CO2)",
    subtitle = paste0("Mauna Loa Observatory "),
    caption = "data source: NOAA
    visualization: Bartosz Czernecki / R climate package",
    x = "",
    y = "ppm"
)

```

![CO2 monthly concentration, Mauna Loa observatory](http://iqdata.eu/kolokwium/co2_chart.svg)

## Example 7
#### Use "climate" inside python environment via rpy2

```python
# load required packages
from rpy2.robjects.packages import importr
import rpy2.robjects as robjects
import pandas as pd
import datetime as dt

# load climate package (make sure that it was installed in R before)
importr("climate")
# test functionality e.g. with meteo_ogimet function for New York - La Guardia:
df = robjects.r["meteo_ogimet"](interval = "daily", station = 72503,
                                date = robjects.StrVector(["2022-05-01", "2022-06-15"]))
# optionally - transform object to pandas data frame and rename columns + fix datetime:
res = pd.DataFrame(df).transpose()
res.columns = df.colnames
res["Date"] = pd.TimedeltaIndex(res["Date"], unit="d") + dt.datetime(1970,1,1)
res.head

>>> res[res.columns[0:7]].head()
#  station_ID       Date TemperatureCAvg  ... TemperatureCMin TdAvgC HrAvg
#0    72503.0 2022-06-15            23.5  ...            19.4   10.9  45.2
#1    72503.0 2022-06-14            25.0  ...            20.6   16.1  59.0
#2    72503.0 2022-06-13            20.4  ...            17.8   16.0  74.8
#3    72503.0 2022-06-12            21.3  ...            18.3   12.0  57.1
#4    72503.0 2022-06-11            22.6  ...            17.8    8.1  40.1

```

## Acknowledgment

Ogimet.com, University of Wyoming, and Institute of Meteorology and Water Management - National Research Institute (IMGW-PIB), National Oceanic & Atmospheric Administration (NOAA) - Earth System Research Laboratory, Global Monitoring Division and Integrated Surface Hourly (NOAA ISH) are the sources of the data.

## Contribution

Contributions to this package are welcome. 
The preferred method of contribution is through a GitHub pull request. 
Feel also free to contact us by creating [an issue](https://github.com/bczernecki/climate/issues).


## Citation

To cite the `climate` package in publications, please use [this paper](https://www.mdpi.com/2071-1050/12/1/394):

Czernecki, B.; Głogowski, A.; Nowosad, J. Climate: An R Package to Access Free In-Situ Meteorological and Hydrological Datasets for Environmental Assessment. Sustainability 2020, 12, 394. https://doi.org/10.3390/su12010394"

LaTeX/BibTeX version can be obtained with:
```
library(climate)
citation("climate")
```
