#' Getting coordinates from a string provided by the Ogimet web portal
#'
#' Internal function for cleaning coordinates' metadata provided by Ogimet
#' @param txt string with coordinates from Ogimet
#' @param pattern which element (Longitude or Latitude) to extract
#'
#' @format The returned object is the geographic coordinates using WGS84 (EPSG:4326) in decimal format.
#' Negative values mean western or southern Hemisphere
#' @keywords internal
#'
#' @examples
#' \donttest{
#'  txt <- "12120:   Leba (Poland)\nLatitude: 54-45N    Longitude: 017-32E    Altitude: 2 m."
#'  climate:::get_coord_from_string(txt, pattern = "Latitude")
#' }
#'
get_coord_from_string <- function(txt, pattern = "Longitude") {
  tt <- gregexpr(pattern, txt)
  start <- tt[[1]][1] + attributes(tt[[1]])$match.length + 1
  tmp <- trimws(substr(txt, start = start, stop = start + 8))
  tmp <- strsplit(tmp, "-")[[1]]
  hemisphere <- gsub("[0-9]", "", strsplit(tmp, "-")[2])
  hemisphere <- gsub(".*?(\\b[A-Za-z0-9 ]+\\b).*","\\1", hemisphere)

  tmp <- suppressWarnings(as.numeric(gsub("([0-9]+).*$", "\\1", strsplit(tmp, "-"))))

  wsp <- suppressWarnings(as.numeric(tmp)[1] + (as.numeric(tmp)[2] * 5 / 3) / 100)

  if( hemisphere %in% c("W","S") ) {
    wsp <- wsp * -1
  }
return(wsp)
}

