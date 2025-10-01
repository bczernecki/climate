#' IMGW hydrological data from the IMGW datastore repository
#'
#' Downloading hourly and sub-hourly (hydrological) data from the telemetric stations
#' available in the danepubliczne.imgw.pl/datastore collection since 2008. 
#' Most parameters are collected with 10 minutes interval and thus it is recommended to download only the mandatory years, parameters or stations.
#' For example, 1 year of data with all available parameters requires processing around 2-4GB of uncompressed data.
#' 
#' Data from the IMGW automated (telemetry) systems are non validated by experts and may contain invalid values.
#' 
#' @param year numeric vector of years to be downloaded (e.g., 2022:2023)
#' @param parameters - character vector describing which parameters to be downloaded. Default `NULL` means to download all available.
#' \enumerate{
#'  \item "level" - Water level (operational) (cm)
#'  \item "level_obs" - Water level (observer) (cm)
#'  \item "flow" - Water flow rate (operational) (m3/s)
#'  \item "temp" - Water temperature (observer) (degree C)
#'  }
#' @param stations - character vector with station name(s) as visible in the `hydro_imgw_telemetry_stations()`.
#' Default `NULL` means to download data for all available stations.
#' @param coords - logical - whether to append the dataset with station full name, longitude, latitude and altitude. Default: TRUE
#' @param allow_failure logical - whether to proceed or stop on failure. By default set to TRUE (i.e. don't stop on error). For debugging purposes change to FALSE
#' @import data.table
#' @export
#' @returns data.table with a raw hydrorological measurements in 10-min or 60-min intervals.
#' Please note that this dataset is not validated by experts and may contain invalid values.
#' @examples 
#' \donttest{
#' imgw_hydro_telemetry = hydro_imgw_datastore(year = 2022,
#'                                             parameters = "flow",
#'                                             stations = "FORDON",
#'                                             coords = TRUE)
#' }
#'


hydro_imgw_datastore = function(year,
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
    tryCatch(hydro_imgw_datastore_bp(year,
                                     parameters,
                                     stations,
                                     coords),
             error = function(e){
               # nocov start
               message(paste("Potential error(s) found. Problems with downloading data.\n",
                             "\rRun function with argument allow_failure = FALSE",
                             "to see more details"))})
  } else {
    hydro_imgw_datastore_bp(year,
                            parameters,
                            stations,
                            coords)
              # nocov end
  }
}

#' @keywords internal
#' @noRd
hydro_imgw_datastore_bp = function(year,
                                   parameters,
                                   stations,
                                   coords) {
  # read metadata for stations:
  telemetry_stations = stations_hydro_imgw_telemetry()
  #telemetry_stations$river = NULL
  
  if (!is.null(stations)) {
    telemetry_stations = telemetry_stations[toupper(telemetry_stations$name) %in% toupper(stations), ]
  }
  
  urls = as.character(
    sapply(year, function(x) paste0(
      "https://danepubliczne.imgw.pl/datastore/getfiledown/Arch/Telemetria/Hydro/",
      x, "/Hydro_", x, "-", sprintf("%02d", 1:12))
    )
  )
  
  dict = data.table::data.table(
    V2 = c("B00020S", "B00014A", "B00050S", "B00101A"),
    parameter = c("level", "level_obs", "flow", "temperature")
  )
  
  if (!is.null(parameters)) {
    dict = dict[dict$parameter %in% parameters,]
  }
  
  all_data = NULL
  
  for (i in seq_along(urls)) {
    
    temp = tempfile()
    tmp_dir = file.path(tempdir(), basename(urls[i]))
    download.file(paste0(urls[i], ".zip"), temp)
    
    if (file.size(temp) > 1000) {
      unzip(zipfile = temp, exdir = tmp_dir)
    } else {
      message("IMGW datastore does not contain data for ", paste(basename(urls[i])), ". skipping\n")
    }
    
    files_to_read = dir(tmp_dir, full.names = TRUE, pattern = paste0(dict$V2, collapse = "|"), recursive = TRUE)
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
      fill = TRUE, use.names = TRUE)
    
    if (!is.null(stations)) {
      all_data[[i]] = all_data[[i]][all_data[[i]]$V1 %in% telemetry_stations$id, ]
    }
    
    unlink(c(temp, tmp_dir), recursive = TRUE)
  }
  
  all_data = data.table::rbindlist(all_data, fill = TRUE, use.names = TRUE)
  all_data = all_data[dict, on = 'V2', param := i.parameter]
  all_data = data.table::dcast(all_data, V1 + V3  ~ param, value.var = "V4")
  all_data$V3 = as.POSIXct(all_data$V3, tz = "UTC")
  
  if (coords) {
    class(telemetry_stations$id) = class(all_data$V1)     # equalise classes of objects:
    all_data = merge(all_data, telemetry_stations, by.x = "V1", by.y = "id", all = FALSE)
    data.table::setcolorder(all_data, c("V1", "name", "river", "lon", "lat", "alt"))
  }
  
  colnames(all_data)[which(colnames(all_data) %in% c("V1", "V3"))] = c("id", "date_time")
  # change class of columns in data.frame to numeric:
  cnames_to_change = colnames(all_data)[colnames(all_data) %in% dict$parameter]
  all_data[, (cnames_to_change) := lapply(.SD, as.numeric), .SDcols = cnames_to_change]
  
  return(all_data)
}
