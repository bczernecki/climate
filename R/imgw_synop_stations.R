#' @name imgw_synop_stations
#' @title Table with codes for SYNOP stations
#'
#' @description The object contains: Polish ID numbers (as used by Polish met service), and names of main (i.e. synop) station in Poland. The table is used for selecting choosen stations to be downloaded and thus saving internal memory.
#'
#'
#' @format The data contains a data.frame with 66 obs. of 2 variables:
#' \itemize{
#'     \item{id} {Station ID}
#'     \item{station} {Station name}
#' }
#'
#' @docType data
#' @keywords datasets meteo
#' @examples
#' data(imgw_synop_stations)
#' head(imgw_synop_stations)
"imgw_synop_stations"
