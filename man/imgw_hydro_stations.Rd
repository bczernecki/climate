% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imgw_hydro_stations.R
\docType{data}
\name{imgw_hydro_stations}
\alias{imgw_hydro_stations}
\title{Location of the hydrological stations from the danepubliczne.imgw.pl collection}
\format{
The data contains a data.frame with 1304 obs. of 3 variables:
\describe{
\item{id}{Station ID}
\item{X}{Longitude}
\item{Y}{Latitude}
}
The object is in the geographic coordinates using WGS84 (EPSG:4326).
}
\usage{
imgw_hydro_stations
}
\description{
The object contains weather stations
coordinates, ID numbers, and elevations
}
\examples{
data(imgw_hydro_stations)
head(imgw_hydro_stations)
}
\keyword{datasets}
\keyword{meteo}
