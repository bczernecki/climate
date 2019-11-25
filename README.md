# climate

[![Build
Status](https://travis-ci.org/bczernecki/climate.png?branch=master)](https://travis-ci.org/bczernecki/climate)
[![CRAN
status](https://www.r-pkg.org/badges/version/climate)](https://cran.r-project.org/package=climate)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/climate)](https://cran.r-project.org/package=climate)

The goal of the  **climate** R package is to automatize downloading of meteorological
and hydrological data from publicly available repositories:

- OGIMET [(ogimet.com)](http://ogimet.com/index.phtml.en) 
- University of Wyoming - atmospheric vertical profiling data (http://weather.uwyo.edu/upperair/).
- Polish Institute of Meterology and Water Management - National Research Institute [(IMGW-PIB)](https://dane.imgw.pl/)

## Installation

~~The stable release of climate package from the [CRAN](https://CRAN.R-project.org) reposity can be installed with:~~

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
It is a wrapper for `meteo_monthly()`, `meteo_daily()`, and `meteo_hourly()` from [the **imgw** package](https://github.com/bczernecki/imgw).

- **sounding_wyoming()** - Downloading measurements of the vertical profile of atmosphere (aka rawinsonde data)
  
### Hydrological data

- **hydro_imgw()** - Downloading hourly, daily, and monthly hydrological data from the SYNOP / CLIMATE / PRECIP stations available in the
danepubliczne.imgw.pl collection.
It is a wrapper for `hydro_annual()`, `hydro_monthly()`, and `hydro_daily()` from [the **imgw** package](https://github.com/bczernecki/imgw).

### Auxiliary functions and datasets

- **stations_ogimet()** - Downloading information about all stations available in the selected
country in the Ogimet repository
- **nearest_stations_ogimet()** - Downloading information about nearest stations to the selected point
available for the selected country in the Ogimet repository
- **meteo_imgw_stations** - Built-in   metadata from the IMGW-PIB repository for   meteorological   stations,   their   geographical
coordinates, and ID numbers
- **hydro_imgw_stations** - Built-in metadata from the IMGW-PIB repository for   hydrological   stations,    their   geographical
coordinates, and ID numbers

## Examples

``` r
library(climate)
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

h = hydro_imgw(interval = "annual", year = 2010)
head(h)
#>             id station riv_or_lake  hyy idyy Mesu idex   H beyy bemm bedd
#> 3223 150210180 ANNOPOL   Wisła (2) 2010   13    H    1 227 2009   12   19
#> 3224 150210180 ANNOPOL   Wisła (2) 2010   13    H    2 319   NA   NA   NA
#> 3225 150210180 ANNOPOL   Wisła (2) 2010   13    H    3 531 2010    3    3
#> 3226 150210180 ANNOPOL   Wisła (2) 2010   14    H    1 271 2010    8   29
#> 3227 150210180 ANNOPOL   Wisła (2) 2010   14    H    1 271 2010   10   27
#> 3228 150210180 ANNOPOL   Wisła (2) 2010   14    H    2 392   NA   NA   NA
```

## Acknowledgment

Ogimet.com, University of Wyoming, and Institute of Meteorology and Water Management - National Research Institute are the sources of the data.

## Contribution

Contributions to this package are welcome. 
The preferred method of contribution is through a GitHub pull request. 
Feel also free to contact us by creating [an issue](https://github.com/bczernecki/climate/issues).
