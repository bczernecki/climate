# climate 1.2.3

* Fixes and modifications for `meteo_imgw_daily()` and `meteo_imgw_hourly()` due
to changes in the IMGW-PIB meteorological datasets since 2024
    * adjusting code to recognize whether downloaded zip file is not corrupted and use the "archive"" package to resolve some of found edge cases
    * skipping corrupted files without breaking the download
    * removed possibility to download stations from IMGW-PIB repository using stations' ID for daily intervals
* Returning all available IMGW-PIB meteorological dataset if provided station name(s) was not found
* Logging errors in package environment and providing summary at the end of downloading instead of instant messaging
* Minor changes in unit-tests to adjust for most recent code changes

# climate 1.2.2

* Fixes and modifications for `hydro_imgw()` set of functions due to changes in the IMGW-PIB hydrological datasets
    * adjusting code to recognize different encoding and directory structure
    * adjusting changes in metadata
    * removed option to download data for "semiannual and annual" time resolutions due to inconsistencies in the data
* Fix unit tests for ogimet- and IMGW-related datasets
* Resolving date formatting for hydrological data - the Date column represents calendar date
* Corrected logic in downloading hourly OGIMET dataset

# climate 1.2.1

* Major fixes for adjusting code to stay in line with CRAN policies
* Corrected duplicated column names for IMGW-PIB stations
* Adjusted encoding changes and documentation updates in `meteo_imgw_telemetry_stations()`

# climate 1.2.0

* Corrected encoding problems for some of platforms where IMGW-PIB metadata were not parsed correctly
* Added location of IMGW-PIB telemetry stations within `meteo_imgw_telemetry_stations()`
* Minor bug fixes


# climate 1.1.1

* Fix problems with downloading `precip` dataset from IMGW-PIB repository after recent changes in metadata
* Bug fix for `ogimet_daily` if data contains more than one year


# climate 1.1.0

* A new approach for handling CRAN policy for resolving problems if network issues are detected or some of the external services are temporarily down. 
* Adding `allow_failure` argument used by default that turns off automatic debugging but avoid warnings and errors for most typical use cases
* re-factoring of unit tests
* documentation build with CI/CD
* updating vignettes and examples, including the way to use climate with Python


# climate 1.0.5

* `meteo_imgw` family of functions supports multiple names as argument - bug fix
* unit tests and code coverage computed with CI/CD
* units added to column labels for `nearest_stations_` set of functions instead of column names for easier processing (e.g. [km] are visible now only as attributes)
* new functions follow lintr settings


# climate 1.0.4

* Function `spheroid_dist` added to improve accuracy of calculations between points, but also avoid installing GIS dependencies (thanks to @kadyb)
* Function `nearest_stations_imgw()` now uses the Vincenty's formula in `spheroid_dist` to calculate the distance between points on a spheroid, not the Euclidean distance (previously results were inaccurate for some specific cases)
* minor bugs fixes and improvements


# climate 1.0.3

* Adding possibility to download BUFR vertical sounding dataset from `http://weather.uwyo.edu/upperair/sounding.html`; extra information with supporting example added to the `sounding_wyoming`'s documentation
* `hydro_imgw` supports now exception for current year which has no flow data until it is verified by the IMGW-PIB
* `ogimet_daily` automatically detects column names to be used for extraction in final data.frame; extra debugging info when temperature or precipitation columns are missing
* minor changes in documentation (e.g. updated links to NOAA website)

# climate 1.0.1

* Adding `data.table` package to read CP1250 on machines that do not support this encoding (translit used instead)

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
