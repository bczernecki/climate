#' IMGW hydrological telemetry stations
#'
#' Retrieving current metadata for hydrological stations used in the telemetric systems of the IMGW-PIB datastore (danepubliczne.imgw.pl/datastore)
#' 
#' @return data table with metadata for over 850 stations. 
#' Metadata contains: station ID, station name, river, latitude, longitude, altitude
#' @importFrom data.table as.data.table
#' @export
#'
#' @examples 
#' \donttest{
#'   hydro_telemetry_stations = stations_hydro_imgw_telemetry()
#' }
#'

stations_hydro_imgw_telemetry = function() {
  
  url = "https://danepubliczne.imgw.pl/datastore/getfiledown/Arch/Telemetria/Hydro/kody_stacji.csv"
  telemetry_stations = tryCatch(expr = suppressWarnings(
    read.csv(url,
             sep = ";",
             stringsAsFactors = FALSE)
  ), 
  error = function(e) {
    message(paste0("Problems with downloading data from:\n", url))
  })
  
  if (!is.null(telemetry_stations)) {
    colnames(telemetry_stations) = c("no", "id", "name", "river", "lat", "lon", "alt")
    # extra fix for columns wrongly signed in IMGW datatabase (i.e. missing semicolon):
    fix_needed = grep(x = substr(telemetry_stations$river, 1, 1), '^[0-9]$')
    
    telemetry_stations[fix_needed, "alt"] = telemetry_stations[fix_needed, "lon"]
    telemetry_stations[fix_needed, "lon"] = telemetry_stations[fix_needed, "lat"]
    telemetry_stations[fix_needed, "lat"] = telemetry_stations[fix_needed, "river"]
    telemetry_stations[fix_needed, "river"] = NA
    
    telemetry_stations$lon = suppressWarnings(coordinates_to_decimal(telemetry_stations$lon))
    telemetry_stations$lat = suppressWarnings(coordinates_to_decimal(telemetry_stations$lat))
    telemetry_stations$alt = as.numeric(gsub(x = telemetry_stations$alt, " ", ""))
    telemetry_stations = as.data.table(telemetry_stations[,-1])
    telemetry_stations$id = as.character(telemetry_stations$id)
  }
  return(telemetry_stations)
}


#' @keywords internal
#' @noRd
coordinates_to_decimal = function(lonlat) {
  converted = unlist(lapply(strsplit(lonlat, " "), function(d) as.numeric(d[[1]]))) +
    unlist(lapply(strsplit(lonlat, " "), function(m) as.numeric(m[[2]]) * 0.01667)) +
    unlist(lapply(strsplit(lonlat, " "), function(s) as.numeric(s[[3]]) * 0.0001667))
  return(converted)
}
