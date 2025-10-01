#' Getting coordinates from a string provided by the Ogimet web portal
#'
#' Internal function for cleaning coordinates' metadata provided by Ogimet
#' @param txt string with coordinates from Ogimet
#' @param pattern which element (Longitude or Latitude) to extract
#'
#' @format The returned object is the geographic coordinates using WGS84 (EPSG:4326) in decimal format.
#' Negative values mean western or southern Hemisphere
#' @keywords internal
#' @noRd

get_coord_from_string = function(txt, pattern = "Longitude") {
  tt_start = gregexpr(pattern, txt)
  start = tt_start[[1]][1] + attributes(tt_start[[1]])$match.length + 1
  tt_stop = gregexpr("N|S|E|W", txt)[[1]]
  stop = tt_stop[which(tt_stop > start)][1]
  tmp = trimws(substr(txt, start = start, stop = stop + 1))
  tmp = strsplit(tmp, "-")[[1]]
  
  hemisphere = gsub("[0-9]", "", strsplit(tmp, "-")[length(tmp)])
  hemisphere = gsub(".*?(\\b[A-Za-z0-9 ]+\\b).*", "\\1", hemisphere)

  tmp = suppressWarnings(as.numeric(gsub("([0-9]+).*$", "\\1", strsplit(tmp, "-"))))
  tmp[3] = ifelse(length(tmp) < 3, 0, tmp[3])
  wsp = suppressWarnings(as.numeric(tmp)[1] + 
                           (as.numeric(tmp)[2] * 5 / 3) / 100 + 
                           (as.numeric(tmp)[3] * 5 / 3) / 100 / 60)

  if (hemisphere %in% c("W", "S")) {
    wsp = wsp * -1
  }
  
  return(wsp)
}
