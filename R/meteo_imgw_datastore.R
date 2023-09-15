#' IMGW meteorological data from IMGW datastore repository
#'
#' Downloading hourly (meteorological) data from the telemetric stations
#' available in the danepubliczne.imgw.pl/datastore collection since 2008
#'
#' @param rank rank of the stations: "synop" (default), "climate", or "precip"
#' @param year vector of years (e.g., 1966:2000)
#' @param allow_failure logical - whether to proceed or stop on failure. By default set to TRUE (i.e. don't stop on error). For debugging purposes change to FALSE
#' @param ... other parameters that may be passed to the 'shortening'
#' function that shortens column names
#' @importFrom utils download.file unzip read.csv
#' @importFrom data.table fread
#' @export
#'
#' @examples \donttest{
#'   #TODO
#' }
#'


meteo_imgw_datastore = function(year,
                             allow_failure = TRUE,
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
                                ...) {
  
  translit = check_locale()
  
  urls = as.character(
    sapply(year, function(x) paste0(
      "https://danepubliczne.imgw.pl/datastore/getfiledown/Arch/Telemetria/Meteo/",
      x, "/Meteo_", x, "-", sprintf("%02d", 1:12))
      )
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
        
        file1 = dir(tmp_dir, full.names = TRUE, pattern = "300S") # meteo
        print(file1)
        
        if (length(file1) && file.exists(file1)) {
        
          if (translit) {
            data1 = data.table::fread(cmd = paste("iconv -f CP1250 -t ASCII//TRANSLIT", file1), dec = ",")
          } else {
            data1 = data.table::fread(file1, header = FALSE, stringsAsFactors = FALSE, dec = ",", sep = ";")
          }
          
          data1$V5 = NULL # status column is always NA
          data1$V2 = NULL # parameter is always known - here: t2m
          colnames(data1) = c("id", "date_time", "t2m")
          data1$date_time = as.POSIXct(data1$date_time, tz = "UTC")
          
          unlink(c(temp, temp2, tmp_dir), recursive = TRUE)
          all_data[[length(all_data) + 1]] = data1
        }
  }
  
  all_data = as.data.frame(do.call(rbind, all_data))
  all_data = all_data[all_data$t2m > -100, ] # remove all unphysical measurements
  
  meteo_telemetry_stations = read.csv("https://danepubliczne.imgw.pl/datastore/getfiledown/Arch/Telemetria/Meteo/kody_stacji.csv",
                                      fileEncoding = "CP1250", sep = ";")
  # 
  # if (coords) {
     all_data = merge(meteo_telemetry_stations, all_data, by.x = "ID", by.y = "id", all.y = TRUE)
  # }
  
  #all_data = all_data[all_data$Rok %in% year, ] # przyciecie tylko do wybranych lat gdyby sie pobralo za duzo
  
  
  # sortowanie w zaleznosci od nazw kolumn - raz jest "kod stacji", raz "id"
  # if (sum(grepl(x = colnames(all_data), pattern = "Kod stacji"))) {
  #   all_data = all_data[order(all_data$`Kod stacji`,
  #                             all_data$Rok,
  #                             all_data$Miesiac,
  #                             all_data$Dzien,
  #                             all_data$Godzina), ]
  # } else {
  #   all_data = all_data[order(all_data$id, all_data$Rok, all_data$Miesiac, all_data$Dzien, all_data$Godzina), ]
  # }
  
  # dodanie opcji  dla skracania kolumn i usuwania duplikatow:
  #all_data = meteo_shortening_imgw(all_data, col_names = col_names, ...)
  return(all_data)
} # end of function
