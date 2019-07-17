#' @name meteo_stations
#' @title Location of the meteorological stations
#'
#' @description The object contains weather stations
#' coordinates, ID numbers, and elevations
#'
#' @format The data contains a data.frame with 1998 obs. of 3 variables:
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
#' data(meteo_stations)
#' head(meteo_stations)
"meteo_stations"
