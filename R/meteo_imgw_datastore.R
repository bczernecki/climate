#' IMGW meteorological data from the IMGW datastore repository
#'
#' Downloading hourly (meteorological) data from the telemetric stations
#' available in the danepubliczne.imgw.pl/datastore collection since 2008. 
#' Most parameters are collected with 10 minutes interval and thus it is recommended to download only the mandatory years, parameters or stations.
#' For example, 1 year of data with all available parameters requires processing around 4GB of uncompressed data.
#' 
#' Data from the IMGW automated (telemetry) systems are non validated by experts and may contain invalid values.
#' 
#'
#' @param year numeric vector of years to be downloaded (e.g., 2022:2023)
#' @param parameters - character vector describing which parameters to be downloaded. Default `NULL` means to download all available.
#' \enumerate{
#'  \item "wd" - wind direction (degrees)
#'  \item "t2m" - temperature at 2 metres above ground level (degree Celsius)
#'  \item "t0m" - ground temperature (degree Celsius)
#'  \item "rr_24h" - precipitation totals for last 24 hours (mm)
#'  \item "rr_1h" - precipitation totals for last 1 hour (mm)
#'  \item "rr_10min" - precipitation totals for last 10 minutes (mm)
#'  \item "ws" - wind speed (m/s)
#'  \item "ws_max" - maximum wind speed for last 10 minutes (m/s)
#'  \item "gust" - wind gust (if present) (m/s)
#'  \item "rh" - relative humidity (%)
#'  \item "water_in_snow" - water equivalent of melted snow cover (mm)
#'  }
#' @param stations - character vector with station names as visible in the `meteo_imgw_telemetry_stations()`.
#' Default `NULL` means to download data for all available stations.
#' @param coords - logical - whether to append the dataset with station full name, longitude, latitude and altitude. Default: TRUE
#' @param allow_failure logical - whether to proceed or stop on failure. By default set to TRUE (i.e. don't stop on error). For debugging purposes change to FALSE
#' @import data.table
#' @export
#' @returns data.frame with a raw meteorological measurements in 10-min intervals
#' @examples 
#' \donttest{
#' imgw_telemetry = meteo_imgw_datastore(year = 2022:2023,
#'                                       parameters = "t2m",
#'                                       stations = c("HALA GĄSIENICOWA",
#'                                                    "DOLINA 5 STAWÓW"),
#'                                       coords = TRUE)
#' }
#'


meteo_imgw_datastore = function(year,
                                parameters = NULL,
                                stations = NULL,
                                coords = TRUE,
                                allow_failure = TRUE) {
  
  # assertions for year
  if (!any(is.character(year) || is.numeric(year))) {
    stop("year argument must be character or numeric")
  }
  
  if (!all(as.numeric(year) >= 2008)) {
    stop("year argument must be provided and all elements must be >= 2008")
  }
  
  if (allow_failure) {
    tryCatch(meteo_imgw_datastore_bp(year,
                                     parameters,
                                     stations,
                                     coords),
             error = function(e){
               message(paste("Potential error(s) found. Problems with downloading data.\n",
                             "\rRun function with argument allow_failure = FALSE",
                             "to see more details"))})
  } else {
    meteo_imgw_datastore_bp(year,
                            parameters,
                            stations,
                            coords)
  }
}

#' @keywords internal
#' @noRd
meteo_imgw_datastore_bp = function(year,
                                   parameters,
                                   stations,
                                   coords) {
  # read metadata for stations:
  telemetry_stations = stations_meteo_imgw_telemetry()
  telemetry_stations$river = NULL
  
  if (!is.null(stations)) {
    telemetry_stations = telemetry_stations[toupper(telemetry_stations$name) %in% toupper(stations), ]
  }
  
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
  
  if (!is.null(parameters)) {
    dict = dict[dict$parameter %in% parameters,]
  }
  
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

    files_to_read = dir(tmp_dir, full.names = TRUE, pattern = paste0(dict$V2, collapse = "|"))
    files_to_read = files_to_read[file.size(files_to_read) > 0]
    
    all_data[[i]] = data.table::rbindlist(
      lapply(files_to_read,
             function(x) data.table::fread(x,
                                           header = FALSE,
                                           stringsAsFactors = FALSE,
                                           dec = ",",
                                           sep = ";",
                                           select = c("V1", "V2", "V3", "V4")
             )
      ),
      fill = TRUE)
    
    if (!is.null(stations)) {
      all_data[[i]] = all_data[[i]][all_data[[i]]$V1 %in% telemetry_stations$id, ]
    }
    
    unlink(c(temp, temp2, tmp_dir), recursive = TRUE)
  }

  all_data = data.table::rbindlist(all_data, fill = TRUE)
  all_data = all_data[dict, on = 'V2', param := i.parameter]
  all_data = data.table::dcast(all_data, V1 + V3  ~ param, value.var = "V4")
  all_data$V3 = as.POSIXct(all_data$V3, tz = "UTC")
  
  if (coords) {
    class(telemetry_stations$id) = class(all_data$V1)     # equalise classes of objects:
    all_data = merge(all_data, telemetry_stations, by.x = "V1", by.y = "id", all = FALSE)
    data.table::setcolorder(all_data, c("V1", "name", "lon", "lat", "alt"))
  }
  
  colnames(all_data)[which(colnames(all_data) %in% c("V1", "V3"))] = c("id", "date_time")
  
  return(all_data)
}
