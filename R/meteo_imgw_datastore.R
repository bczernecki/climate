#' IMGW meteorological data from the IMGW datastore repository
#'
#' Downloading hourly (meteorological) data from the telemetric stations
#' available in the danepubliczne.imgw.pl/datastore collection since 2008. 
#' Most parameters are collected within 10 minutes interval and thus it is advised to download only the required subsets.
#' Data from the IMGW automated (telemetry) systems are non validated by experts and may contain not valid measurements.
#' 
#'
#' @param year vector of years (e.g., 1966:2000)
#' @param parameters 

#' @param allow_failure logical - whether to proceed or stop on failure. By default set to TRUE (i.e. don't stop on error). For debugging purposes change to FALSE
#' @param ... other parameters that may be passed to the 'shortening'
#' function that shortens column names
#' @import data.table
#' @export
#'
#' @examples \donttest{
#'   #TODO
#' }
#'


meteo_imgw_datastore = function(year,
                                allow_failure = TRUE,
                                parameters = NULL,
                                ...) {
  
  if (allow_failure) {
    tryCatch(meteo_imgw_datastore_bp(year,
                                     ...),
             warning = function(w) {
               message(paste("Potential problem(s) found. Problems with downloading data.\n",
                             "\rRun function with argument allow_failure = FALSE",
                             "to see more details"))
             },
             error = function(e){
               message(paste("Potential error(s) found. Problems with downloading data.\n",
                             "\rRun function with argument allow_failure = FALSE",
                             "to see more details"))})
  } else {
    meteo_imgw_datastore_bp(year,
                            ...)
  }
}

#' @keywords internal
#' @noRd
meteo_imgw_datastore_bp = function(year,
                                   parameters = NULL,
                                   ...) {
  
  urls = as.character(
    sapply(year, function(x) paste0(
      "https://danepubliczne.imgw.pl/datastore/getfiledown/Arch/Telemetria/Meteo/",
      x, "/Meteo_", x, "-", sprintf("%02d", 1:12))
    )
  )
  
  dict = data.table::data.table(
    V2 = c("B00202A", "B00300S", "B00305A", "B00604S", "B00606S",
           "B00608S", "B00702A", "B00703A", "B00714A", "B00802A",
           "B00910A"),
    parameter = c("wd", "t2m", "t0m", "rr_24h", "rr_1h",
                  "rr_10min", "ws", "ws_max", "gust", "rh",
                  "water_in_snow")
  )
  
  all_data = NULL
  
  for (i in seq_along(urls)) {
    temp = tempfile()
    temp2 = tempfile()
    tmp_dir = file.path(tempdir(), basename(urls[i]))
    download.file(paste0(urls[i], ".zip"), temp)
    download.file(paste0(urls[i], ".ZIP"), temp2)
    
    if (file.size(temp) > 1000) {
      unzip(zipfile = temp, exdir = tmp_dir)
    } else if (file.size(temp2) > 1000) {
      unzip(zipfile = temp2, exdir = tmp_dir)
    } else {
      message("IMGW datastore does not contain data for ", paste(basename(urls[i])), ". skipping\n")
    }
    
    all_data[[i]] = data.table::rbindlist(
      lapply(dir(tmp_dir, full.names = TRUE), 
             function(x) data.table::fread(x,
                                           header = FALSE,
                                           stringsAsFactors = FALSE,
                                           dec = ",",
                                           sep = ";",
                                           select = c("V1", "V2", "V3", "V4")
             )
      ),
      fill = TRUE)
    
    if (!is.null(parameters) && nrow(all_data[[i]])) {
      params = dict[parameter %in% parameters, V2]
      all_data[[i]] = all_data[[i]][V2 %in% params, ]
    }
    
    unlink(c(temp, temp2, tmp_dir), recursive = TRUE)
  }
  
  all_data = data.table::rbindlist(all_data, fill = TRUE)
  all_data[dict, on = 'V2', param := i.parameter]
  all_data = data.table::dcast(all_data, V1 + V3  ~ param, value.var = "V4")
  all_data$V3 = as.POSIXct(all_data$V3, tz = "UTC")
  
  # polaczyc ze wspolrzednymi i nazwami stacji
  
  
  unlink(c(temp, temp2, tmp_dir), recursive = TRUE)
  all_data[[length(all_data) + 1]] = data1
}

all_data = as.data.frame(do.call(rbind, all_data))
all_data = all_data[all_data$t2m > -100, ] # remove all unphysical measurements

telemetry_stations = read.csv("https://danepubliczne.imgw.pl/datastore/getfiledown/Arch/Telemetria/Meteo/kody_stacji.csv",
                              fileEncoding = "CP1250",
                              sep = ";")
colnames(telemetry_stations) = c("no", "id", "name", "river", "lat", "lon", "alt")
telemetry_stations$lon = coordinates_to_decimal(telemetry_stations$lon)
telemetry_stations$lat = coordinates_to_decimal(telemetry_stations$lat)
telemetry_stations$alt = as.numeric(gsub(x = telemetry_stations$alt, " ", ""))
telemetry_stations = as.data.table(telemetry_stations[,-1])
telemetry_stations$id = as.character(telemetry_stations$id)
res = merge(all_data, telemetry_stations, by.x = "V1", by.y = "id", all = FALSE)
# 
# if (coords) {
#   all_data = merge(meteo_telemetry_stations, all_data, by.x = "ID", by.y = "id", all.y = TRUE)
# }

#all_data = all_data[all_data$Rok %in% year, ] # przyciecie tylko do wybranych lat gdyby sie pobralo za duzo


# sortowanie w zaleznosci od nazw kolumn - raz jest "kod stacji", raz "id"

#   all_data = all_data[order(all_data$id, all_data$Rok, all_data$Miesiac, all_data$Dzien, all_data$Godzina), ]
return(all_data)
} # end of function


#' @keywords internal
#' @noRd
meteo_imgw_datastore_read = function(tmp_dir, pattern = "300S", param = "t2m") {
  csv_path = dir(tmp_dir, full.names = TRUE, pattern = pattern)
  if (length(csv_path) && file.exists(csv_path)) {
    data = data.table::fread(csv_path,
                             header = FALSE,
                             stringsAsFactors = FALSE, dec = ",",
                             sep = ";",
                             select = c("V1", "V3", "V4"))
    
    colnames(data) = c("id", "date_time", param)
    data$date_time = as.POSIXct(data$date_time, tz = "UTC")
  } else {
    data = data.frame(id = NA, date_time = NA, param = NA)
    colnames(data) = c("id", "date_time", param)
  }
  return(data)
}


coordinates_to_decimal = function(lonlat) {
  converted = unlist(lapply(strsplit(lonlat, " "), function(d) as.numeric(d[[1]]))) +
    unlist(lapply(strsplit(lonlat, " "), function(m) as.numeric(m[[2]]) * 0.01667)) +
    unlist(lapply(strsplit(lonlat, " "), function(s) as.numeric(s[[3]]) * 0.0001667))
  return(converted)
}
