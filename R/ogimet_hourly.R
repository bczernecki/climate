#' Scrapping hourly meteorological (Synop) data from the Ogimet webpage
#'
#' Downloading hourly (meteorological) data from the Synop stations available in the https://www.ogimet.com/ repository
#'
#' @param date start and finish of date (e.g., date = c("2018-05-01","2018-07-01") )
#' @param coords add geographical coordinates of the station (logical value TRUE or FALSE)
#' @param station WMO ID of meteorological station(s). Character or numeric vector
#' @param precip_split whether to split precipitation fields into 6/12/24h
#'  numeric fields (logical value TRUE (default) or FALSE)
#' @importFrom XML readHTMLTable
#' 
#' @export
#' 
#' @keywords internal
#'
#' @examples 
#' \donttest{
#'   # downloading data for Poznan-Lawica
#'   poznan <- ogimet_hourly(station = 12330, coords = TRUE, precip_split = TRUE)
#'   head(poznan)
#' }
#'

ogimet_hourly <- function(date = c("2019-06-01","2019-07-31"), coords = FALSE, station = c(12326, 12330),  precip_split = TRUE){

  #options(RCurlOptions = list(ssl.verifypeer = FALSE)) # required on windows for RCurl

  dates <- seq.Date(min(as.Date(date)), max(as.Date(date)), by = "1 month") - 1
  dates <- unique(c(dates, as.Date(max(date))))

  # initalizing empty data frame for storing results:
  data_station <-
    data.frame(
      "Date" = character(),
      "hour" = character(),
      "TC" = character(),
      "TdC" = character(),
      "TmaxC" = character(),
      "TminC" = character(),
      "ddd" = character(),
      "ffkmh" = character(),
      "Gustkmh" = character(),
      "P0hPa" = character(),
      "PseahPa" = character(),
      "PTnd" = character(),
      "Precmm" = character(),
      "Nt" = character(),
      "Nh" = character(),
      "HKm" = character(),
      "InsoD1" = character(),
      "Viskm" = character(),
      "Snowcm" = character(),
      "WW" = character(),
      "W1" = character(),
      "W2" = character(),
      stringsAsFactors = FALSE
    )


  for (station_nr in station){
    print(station_nr)
    # adding progress bar if at least 3 iterations are needed
    if(length(dates)*length(station) >=3 ) pb <- txtProgressBar(min = 0, max = length(dates)*length(station)-1, style = 3)
    
#    print(station_nr)
    for (i in length(dates):1) {
      
      if(length(dates) >=3 ) paste(setTxtProgressBar(pb, abs(length(dates)*length(station) - i)),"\n")
      
      year <- format(dates[i], "%Y")
      month <- format(dates[i], "%m")
      day <- format(dates[i], "%d")
      ndays <- day
      linkpl2 <- paste("https://www.ogimet.com/cgi-bin/gsynres?ind=",station_nr,"&lang=en&decoded=yes&ndays=",ndays,"&ano=",year,"&mes=",month,"&day=",day,"&hora=23",sep="")
      if(month=="01") linkpl2 <- paste("http://ogimet.com/cgi-bin/gsynres?ind=",station_nr,"&lang=en&decoded=yes&ndays=31&ano=",year,"&mes=02&day=1&hora=00",sep="")
      
      
      
      temp = tempfile()
      test_url(linkpl2, temp)
      
      # run only if downloaded file is valid
      if(!is.na(file.size(temp)) & (file.size(temp) > 0)) { 
      
        #a <- getURL(linkpl2)
        a <- readHTMLTable(temp, stringsAsFactors = FALSE)
        unlink(temp)
        
        #a <- readHTMLTable(a, stringsAsFactors=FALSE)
  
        b <-  a[[length(a)]]
        
        if (is.null(b)) {
          warning(paste0("Wrong station ID: ", station_nr, " You can check station ID at https://ogimet.com/display_stations.php?lang=en&tipo=AND&isyn=&oaci=&nombre=&estado=&Send=Send"))
          return(data_station)
          } 
        
        colnames(b) <- gsub("[^A-Za-z0-9]", "", as.character(lapply(b[1, ], as.character), stringsAsFactors = FALSE))
        colnames(b) <- c("Date", "hour", colnames(b)[2:(ncol(b) - 1)]) # workaround for adding hour which is wrongly recognized
        b <- b[-1, ]
        b["station_ID"] <-  station_nr
        # to avoid gtools::smartbind function or similar from another package..
        if (ncol(data_station) >= ncol(b)) {
          b[setdiff(names(data_station), names(b))] <- NA # adding missing columns
          data_station <- rbind(data_station, b)  # joining data
  
        } else { # when b have more columns then data_station
          if(nrow(data_station) == 0){
            data_station = b
          } else {
            # adding missing columns
            data_station <- merge(b, data_station, all = TRUE)# joining data
          }
  
        }
  
        #cat(paste(year, month, "\n"))
  
        # coords można lepiej na samym koncu dodać kolumne
        # wtedy jak zmienia się lokalizacja na dacie to tutaj tez
        if (coords){
          coord <- a[[1]][2,1]
          data_station["Lon"] <- get_coord_from_string(coord, "Longitude")
          data_station["Lat"] <- get_coord_from_string(coord, "Latitude")
        }
        
      } # end of checking for empty files / problems with connection
      
    } # end of looping for dates
    
  }# end of looping for stations
  
  if(nrow(data_station) > 0){
  
      data_station <- data_station[!duplicated(data_station), ]
  
    # converting character to proper field representation:
  
    # get rid off "---" standing for missing/blank fields:
    data_station[which(data_station == "--" | data_station == "---" | data_station == "----" | data_station == "-----", arr.ind = TRUE)] <- NA
  
    # changing time..
    data_station$Date <-strptime(paste(data_station$Date, data_station$hour), "%m/%d/%Y %H:%M", tz = 'UTC')
    data_station$hour <- NULL
  
    # other columns to numeric:
    suppressWarnings(data_station[, c("TC", "TdC", "ffkmh",  "Gustkmh", "P0hPa", "PseahPa", "PTnd", "Nt", "Nh",
                    "HKm", "InsoD1", "Viskm", "Snowcm","station_ID")] <-
      as.data.frame(sapply(data_station[,c("TC", "TdC", "ffkmh", "Gustkmh", "P0hPa", "PseahPa", "PTnd", "Nt","Nh",
                                           "HKm", "InsoD1", "Viskm", "Snowcm","station_ID")], as.numeric)))
  
    #  TODO:
    # changing order of columns and removing blank records:
    if(coords){
      ord1 <- c("station_ID", "Lon", "Lat", "Date", "TC")
      ord1 <- c(ord1, setdiff(names(data_station), c("station_ID", "Lon", "Lat", "Date", "TC")))
      ord1 <- ord1[!(ord1 %in% c("WW", "W1","W2","W3"))]
      data_station <- data_station[, ord1]
    } else {
      ord1 <- c("station_ID", "Date", "TC")
      ord1 <- c(ord1, setdiff(names(data_station), c("station_ID", "Date", "TC")))
      ord1 <- ord1[!(ord1 %in% c("WW", "W1","W2","W3"))]
      data_station <- data_station[, ord1]
    }
    # setdiff(names(df), c("station_ID", "Date", "TC"))
  
  
    # splitting precipitation into 6-12-24 hours from a default string in the Precmm column:
    if(precip_split){
      data_station$pr6 <- precip_split(data_station$Precmm, pattern = "/6")
      data_station$pr12 <- precip_split(data_station$Precmm, pattern = "/12")
      data_station$pr24 <- precip_split(data_station$Precmm, pattern = "/24")
    }
  
    # clipping to interesting period as we're downloading slightly more than needed:
    data_station <- data_station[which(as.Date(data_station$Date) >= as.Date(min(date)) & as.Date(data_station$Date) <= as.Date(max(date))), ]
  
    } # end of checking whether object is empty

  return(data_station)

}
