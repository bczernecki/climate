# climate 0.9.9

* Changing URL `danepubliczne.imgw.pl` to `dane.imgw.pl` where needed
* Fixing minor ogimet and IMGW bugs
* Do not stop downloading data from `ogimet.com`, instead check for all available data in given period of time

# climate 0.9.8

* Adding informative message if problems with NOAA hourly dataset occur
* Informative message if problems with downloading detected for non-IMGW dataset

# climate 0.9.7

* stop working if no internet connection detected

# climate 0.9.6

* Adding nearest_stations_noaa for NOAA hourly dataset

# climate 0.9.5

* Following CRAN policies
    * Adding information if connection issues detected or URL no accessible
    * RCurl dependency removal
    * CO2 & Wyoming examplary data can be loaded offline

# climate 0.9.4

* New dataset:
    * Hourly NOAA ISH (Integrated Surface Hourly) data - global meteorological dataset dated back up to 1900

# climate 0.9.3

* Bug fixes
    * #27
* New datasets:
    * CO2 concentration from Mauna Loa observatory

# climate 0.9.2

* Bug fixes
    * #26

# climate 0.9.1

* climate is independent of imgw package
* Bug fixes
    * #24
* restored possibility of downloading single station from Polish (IMGW) repository

# climate 0.3

* improves API

# climate 0.2

* splits imgw into two packages: imgw and climate

# imgw 0.1.1

* New datasets
    * Synop data from ogimet (http://ogimet.com/index.phtml.en)
* New functions
* adding function for reading station's coordinates from "Ogimet" webportal
    * `ogimet()` - downloading Synop hourly or monthly data from the "Ogimet" webportal
        * `ogimet_hourly()` - downloading Synop hourly data from the "Ogimet" webportal
        * `ogimet_daily()` - downloading Synop daily aggregates from the "Ogimet" webportal 
    * `ogimet_stations()` - retrieving geographical coordinates, altitude, WMO IDs and station names for the user-specified country nam; optionally plot results on a map
* Improvements
* Bug fixes
    * Fixes a bug in the `hydro_daily()` that prevented from merging more than 1 dataset

# imgw 0.1.0

* Deploying the package on CRAN!
* New functions
    * New function `meteo()` for downloading monthly, daily, and hourly meteorological data
    * New function `meteo_monthly()` for downloading monthly meteorological data
    * New function `meteo_daily()` for downloading daily meteorological data
    * New function `meteo_hourly()` for downloading hourly meteorological data
    * New function `hydro()` for downloading semiannual and annual, monthly, and daily hydrological data
    * New function `hydro_annual()` for downloading semiannual and annual hydrological data
    * New function `hydro_monthly()` for downloading monthly hydrological data
    * New function `hydro_daily()` for downloading daily hydrological data
    * New function `meteo_metadata()` for downloading the metadata of the meteorological data
    * New function `hydro_metadata()` for downloading the metadata of the hydrological data
    * New function `meteo_sounding()` for downloading the mea (i.e. measurements of the vertical profile of atmosphere) sounding data
* New datasets
    * New dataset `meteo_stations` containing Polish meteorological station's localizations
    * New dataset `hydro_stations` containing Polish hydrological station's localizations
* Improvements
    * Added a `NEWS.md` file to track changes to the package.
