#' Hourly NOAA Integrated Surface Hourly (ISH) meteorological data
#'
#' Downloading hourly (meteorological) data from the SYNOP stations available in the NOAA ISD collection.
#' Some stations in the dataset are dated back even up to 1900.
#' By default only records that follow FM-12 (SYNOP) convention are processed.
#' Further details available at: https://www1.ncdc.noaa.gov/pub/data/noaa/readme.txt
#'
#' @param station ID of meteorological station(s) (characters). Find your station's ID at: https://www1.ncdc.noaa.gov/pub/data/noaa/isd-history.txt
#' @param year vector of years (e.g., 1966:2000)
#' @param fm12 use only FM-12 (SYNOP) records (TRUE by default)
#' @importFrom utils download.file unzip read.csv
#' @export
#'
#' @examples \donttest{
#'   noaa = meteo_noaa_hourly(station = "123300-99999", 
#'                            year = 2019) # poznan, poland
#'   head(noaa)
#' }
#'

meteo_noaa_hourly <- function(station = NULL, year, fm12 = TRUE){
  
  stopifnot(is.character(station)) 
  #options(RCurlOptions = list(ssl.verifypeer = FALSE)) # required on windows for RCurl
  
  base_url <- "https://www1.ncdc.noaa.gov/pub/data/noaa/"
  
  all_data <- NULL
  
  for (i in seq_along(year)){

      address = paste0(base_url, year[i], "/", station, "-", year[i], ".gz")
      temp = tempfile()
      test_url(address, temp)
      
      # run only if downloaded file is valid
      dat = NULL
      if(!is.na(file.size(temp)) & (file.size(temp) > 0)) { 
      
      dat = read.fwf(gzfile(temp,'rt'),header=F,  
                   c(4, 6, 5, 4, 2, 2, 2, 2, 1, 6, 
                     7, 5, 5, 5, 4, 3, 1, 1, 4, 1,
                     5, 1, 1, 1, 6, 1, 1, 1, 5, 1, 5, 1, 5, 1)) 
      unlink(temp)
      
      if(fm12){
      dat = dat[dat$V12 == "FM-12",] # take only FM-12 records
      }
      
      dat = dat[, c(4:7, 10:11, 13, 16, 19, 25, 29, 31, 33)]
      colnames(dat) = c("year", "month", "day", "hour", "lat", "lon", "alt",
                        "wd", "ws",  "visibility", "t2m", "dpt2m", 
                        "slp")
      
      dat$date = ISOdatetime(year = dat$year, 
                             month = dat$month, 
                             day = dat$day, 
                             hour = dat$hour, 0, 0, tz = "UTC")
                            
      
      dat$t2m[dat$t2m == 9999] = NA
      dat$dpt2m[dat$dpt2m == 9999] = NA
      dat$ws[dat$ws == 9999] = NA
      dat$wd[dat$wd == 999] = NA
      dat$slp[dat$slp == 99999] = NA
      dat$visibility[dat$visibility == 999999] = NA
      
      dat$lon = dat$lon/1000
      dat$lat = dat$lat/1000
      dat$ws = dat$ws/10
      dat$t2m = dat$t2m/10
      dat$dpt2m = dat$dpt2m/10
      dat$slp = dat$slp/10
      
      } else {
        
       cat(paste0("  Check station name or year. The created link is not working properly:\n  ", address))
        
      }  # end of if statement for empty files
                                                                                                                                                                                                                                                                                                                                      
        
      all_data[[length(all_data) + 1]] <- dat
      } # end of loop for years
    
  if(is.list(all_data)){
    all_data <- do.call(rbind, all_data)
  }
  
  
  if(!is.null(all_data)){ # run only if there are some data downloaded:
  
    # order columns:
    all_data = all_data[, c("date","year", "month", "day", "hour", "lon", "lat", "alt",
                            "t2m", "dpt2m", "ws", "wd", "slp", "visibility") ]
    
    # sort data
    all_data = all_data[order(all_data$date), ]
    
    #closeAllConnections() # just in case sth left while testing...
  }
  
  return(all_data)
} # koniec funkcji meteo_terminowe

