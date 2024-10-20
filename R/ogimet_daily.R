#' Scrapping daily meteorological (Synop) data from the Ogimet webpage
#'
#' Downloading daily (meteorological) data from the Synop stations available in the https://www.ogimet.com/ repository.
#' The data are processed only if temperature or precipitation fields are present.
#'
#' @param date start and finish of date (e.g., date = c("2018-05-01","2018-07-01") ). By default last 30 days.
#' @param coords add geographical coordinates of the station (logical value TRUE or FALSE)
#' @param station WMO ID of meteorological station(s). Character or numeric vector
#' @param hour time for which the daily raport is generated. Set default as hour = 6 (i.e. 6 UTC)
#' @param allow_failure logical - whether to proceed or stop on failure. By default set to TRUE (i.e. don't stop on error). For debugging purposes change to FALSE
#' @importFrom XML readHTMLTable
#' @importFrom utils setTxtProgressBar txtProgressBar
#' 
#' @export
#' @returns data.frame with historical meteorological data for the daily summaries
#'
#' @examples 
#' \donttest{
#'   # downloading daily summaries for last 30 days. station: New York - La Guardia
#'   new_york = ogimet_daily(station = 72503, coords = TRUE)
#' }
#'

ogimet_daily = function(date = c(Sys.Date() - 30, Sys.Date()),
                        coords = FALSE, 
                        station = NA, 
                        hour = 6, 
                        allow_failure = TRUE) {
  
  if (allow_failure) {
    tryCatch(ogimet_daily_bp(date = date, coords = coords, station = station, hour = hour), 
             error = function(e){
               message(paste("Problems with downloading data.",
                             "Run function with argument allow_failure = FALSE",
                             "to see more details"))})
  } else {
    ogimet_daily_bp(date = date, coords = coords, station = station, hour = hour)
  }
}

#' @keywords Internal
#' @noRd
ogimet_daily_bp = function(date = date,
                        coords = coords, 
                        station = station, 
                        hour = hour) {
  dates = seq.Date(min(as.Date(date)), max(as.Date(date)), by = "1 month") 
  dates = unique(c(dates, as.Date(max(date))))

  # initalizing empty data frame for storing results:
  message(
    paste(
      "Daily raports will be generated for", hour, 
      "UTC each day. Use the >>hour<< argument to change it \n"
    )
  )
  data_station <-
    data.frame(
      "Date" = character(),
      "TemperatureCMax" = character(),
      "TemperatureCMin" = character(),
      "TemperatureCAvg" = character(),
      "TdAvgC" = character(),
      "HrAvg" = character(),
      "WindkmhDir" = character(),
      "WindkmhInt" = character(),
      "WindkmhGust" = character(),
      "PresslevHp" = character(),
      "PreselevHp" = character(),
      "Precmm" = character(),
      "SunD1h" = character(),
      "SnowDepcm" = character(),
      "TotClOct" = character(),
      "lowClOct" = character(),
      "station_ID" = character(),
      "VisKm" = character(),
      stringsAsFactors = FALSE
    )
  
  for (station_nr in station) {
    message(station_nr)
    
    # adding progress bar if at least 3 iterations are needed
    if (length(dates) * length(station) >= 3) {
      pb = txtProgressBar(min = 0, max = length(dates) * length(station) - 1, style = 3)
    } 

    for (i in length(dates):1) {
      # update progressbar:
      if (length(dates) >= 3) paste(setTxtProgressBar(pb, abs(length(dates)*length(station) - i)), "\n")
      
      year = format(dates[i], "%Y")
      month = format(dates[i], "%m")
      day = format(dates[i], "%d")
      ndays = day

      linkpl2 = paste("https://www.ogimet.com/cgi-bin/gsynres?lang=en&ind=", 
                      station_nr, "&ndays=32&ano=", 
                      year, "&mes=", month, "&day=", day, 
                      "&hora=", hour,"&ord=REV&Send=Send", sep = "")
      temp = tempfile()
      test_url(linkpl2, temp)
      if (is.na(file.size(temp)) | (file.size(temp) < 500)) { 
        message("Problem with downloading data from:", linkpl2, "\n")
        if (exists("data_station")) {
          message("Returning results downloaded up to this point:\n")
          return(data_station)
        }
      } else { # run only if downloaded file is valid
        
        a = readHTMLTable(temp, stringsAsFactors = FALSE)
        unlink(temp)
        
        b = a[[length(a)]]
        
        # check no data situations:
        tst = sum(grepl(x = b[[1]], pattern = "No valid data"))
        if (tst) {
          message(paste(b[[1]], dates[i]))
        } else {
        
          if (sum(b[1, ] == "Dailyweather summary", na.rm = TRUE)) {
            b = b[, 1:(length(b) - 8)]
          } else {
            b = b[, 1:length(b)]
          }
          
          test = b[1:2, ]
          
          if (is.null(test) ) {
            warning(paste0("Wrong station ID: ", station_nr,
                           " You can check available stations ID at https://ogimet.com/display_stations.php?lang=en&tipo=AND&isyn=&oaci=&nombre=&estado=&Send=Send"))
            return(data_station)
          } 
          
          # all columns are empty:
          if (all(is.na(test[2, ]))) {
            message("no values in column names")
          } else {
            
            # number of columns contain weird/non-standard data (e.g. only wind speed)
            if (ncol(test) <= 4) {
              message(paste0("Mandatory meteorological parameters (i.e. Temperature or precipitations) are not present. \nCheck content of the data using current URL:\n",
                             linkpl2))
              return(test)
            }
            
            if ((length(test[2, !is.na(test[2, ])]) == 6 &
                 test[2, 5] == "Int.")) {
              names_col = unlist(c(
                test[1, 1],
                paste(test[1, 2], test[2, 1:3], sep = "_"),
                test[1, 3:4],
                paste(test[1, 5], test[2, 4:6], sep = "_"),
                test[1, c(6:(length(test) - 4))]
              ))
            } else if ((length(test[2, !is.na(test[2, ])]) == 2 & test[2, 2] == "Int.")) {
              names_col = unlist(c(test[1, 1:2], paste(test[1, 3], test[2, 1:2], sep = "_"), test[1, c(4:(length(test) - 1))]))
            } else if ((length(test[2, !is.na(test[2, ])]) == 5 & test[2, 5] == "Int.")) {
              names_col = unlist(c(
                test[1, 1],
                paste(test[1, 2], test[2, 1:3], sep = "_"),
                test[1, 3:4],
                paste(test[1, 5], test[2, 4:5], sep = "_"),
                test[1, c(6:(length(test) - 3))]
              ))
            } else if ((length(test[2, !is.na(test[2, ])]) == 3 & test[2, 2] == "Int.")) {
              names_col = unlist(c(
                test[1, 1:2],
                paste(test[1, 3], test[2, 1:3], sep = "_"),
                test[1, c(4:(length(test) - 2))]
              ))
            } else {
              names_col = "Error_column"
            }

          names_col =
            gsub("[^A-Za-z0-9]",
                 "",
                 as.character(lapply(names_col, as.character), stringsAsFactors = FALSE))
    
          colnames(b) = names_col
          b = b[-c(1:2), ]
          b["station_ID"] = station_nr
    
          # extra check if date is for December and January simultanously
          # e.g. "01/02" "01/01" "12/31" "12/30"
          uniq_mths = sort(unique(unlist(lapply(strsplit(b$Date, "/"), "[[", 1))))
          if (sum(uniq_mths %in% c("01", "12")) == 2) {
            mth = unlist(lapply(strsplit(b$Date, "/"), "[[", 1))
            yr = ifelse(mth == "01", as.numeric(year), as.numeric(year) - 1)
            b$Date = as.character(paste0(b$Date, "/", yr))
          } else {
            b$Date = as.character(paste0(b$Date, "/", year))
          }

          # to avoid gtools::smartbind function or similar from another package..
          if (ncol(data_station) >= ncol(b)) {
            b[setdiff(names(data_station), names(b))] = NA # adding missing columns
            data_station = rbind(data_station, b)  # joining data
    
          } else { # when b have more columns then data_station
           if (nrow(data_station) == 0) {
             data_station = b
            } else {
              # adding missing columns
              data_station = merge(b, data_station, all = TRUE)# joining data
            }
    
            }
    
          if (coords) {
            coord = a[[1]][2, 1]
            data_station["Lon"] = get_coord_from_string(coord, "Longitude")
            data_station["Lat"] = get_coord_from_string(coord, "Latitude")
          }
          
              } # end of checking for: no values in column names
    
            } # end of checking for: No valid data found in database
    
          } # end of checking for empty files / problems with connection
      
      } # end of looping for dates

    }# end of looping for stations
    
    if (nrow(data_station) > 0) {
      
      data_station =  data_station[!duplicated(data_station), ]
      
    # converting character to proper field representation:
  
    # get rid off "---" standing for missing/blank fields:
    data_station[which(data_station == "--" | data_station == "---" | data_station == "----" | data_station == "-----", arr.ind = TRUE)] = NA
  
    # other columns to numeric:
    suppressWarnings(data_station[,c("TemperatureCMax", "TemperatureCMin", "TemperatureCAvg","TdAvgC" ,"HrAvg",
                                     "WindkmhInt","WindkmhGust" ,"PresslevHp", "Precmm" ,
                                     "TotClOct", "lowClOct" ,"VisKm","station_ID")] <-
                       as.data.frame(sapply(data_station[,c("TemperatureCMax", "TemperatureCMin", "TemperatureCAvg","TdAvgC" ,"HrAvg",
                                                            "WindkmhInt","WindkmhGust" ,"PresslevHp", "Precmm" ,
                                                            "TotClOct", "lowClOct" ,"VisKm","station_ID")], as.numeric)))
    
    # changing order of columns and removing blank records:
    if (coords) {
      ord1 = c("station_ID", "Lon", "Lat", "Date", "TemperatureCAvg")
      ord1 = c(ord1, setdiff(names(data_station), c("station_ID", "Lon", "Lat", "Date", "TemperatureCAvg")))
      data_station = data_station[, ord1]
    } else {
      ord1 = c("station_ID", "Date", "TemperatureCAvg")
      ord1 = c(ord1, setdiff(names(data_station), c("station_ID", "Date", "TemperatureCAvg")))
      data_station = data_station[, ord1]
    }
    
    # date to as.Date()
    data_station$Date = as.Date(as.character(data_station$Date), format = "%m/%d/%Y")
    # clipping to interesting period as we're downloading slightly more than needed:
    data_station = data_station[which(data_station$Date >= as.Date(min(date)) & as.Date(data_station$Date) <= as.Date(max(date))), ]
    
  } # end of checking whether no. of rows > 0 
  
  # removing duplicates:
  data_station = data_station[row.names(unique(data_station[, c("station_ID", "Date")])), ]
  return(data_station)
}
