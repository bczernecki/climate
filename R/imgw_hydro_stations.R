#' @name imgw_hydro_stations
#' @title Location of the hydrological stations from the dane.imgw.pl collection
#'
#' @description The object contains weather stations
#' coordinates, ID numbers, and elevations
#'
#' @format The data contains a data.frame with 1304 obs. of 3 variables:
#' \itemize{
#'     \item{id} {Station ID}
#'     \item{X} {Longitude}
#'     \item{Y} {Latitude}
#' }
#' The object is in the geographic coordinates using WGS84 (EPSG:4326).
#'
#' @docType data
#' @keywords datasets meteo
#' @examples
#' data(imgw_hydro_stations)
#' head(imgw_hydro_stations)
"imgw_hydro_stations"
