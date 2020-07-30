#' Scrapping a list of meteorological (Synop) stations for a defined country from the Ogimet webpage
#'
#' Returns a list of meteorological stations with their coordinates from the Ogimet webpage. The returned list is valid only for a given day
#'
#' @param country country name; for more than two words they need to be seperated with a plus character (e.g. "United+Kingdom")
#' @param date a day when measurements were done in all available locations
#' @param add_map logical - whether to draw a map with downloaded metadata (requires maps/mapdata packages)
#' @importFrom XML readHTMLTable
#' @export
#' @return A data.frame with columns describing the synoptic stations in selected countries where each row represent a statation.
#' If `add_map = TRUE` additional map of downloaded data is added.  
#'
#' @examples 
#' \donttest{
#'   stations_ogimet(country = "Australia", add_map = TRUE)
#' }
#'
stations_ogimet <- function(country = "United+Kingdom", date = Sys.Date(), add_map = FALSE){
  
  #options(RCurlOptions = list(ssl.verifypeer = FALSE)) # required on windows for RCurl
  
  if (length(country)!=1) {
    stop("To many country selected. Please choose one country")
  }
  
  if (length(date)!=1) {
    stop("You can check available nearest stations for one day. Please chenge selection")
    
  }  # initalizing empty data frame for storing results:
  
  year <- format(date, "%Y")
  month <- format(date, "%m")
  day <- format(date, "%d")
  ndays <- 1
  linkpl2 <- paste0("http://ogimet.com/cgi-bin/gsynres?lang=en&state=",country,"&osum=no&fmt=html&ord=REV&ano=",year,"&mes=",month,"&day=",day,"&hora=06&ndays=1&Send=send")
  
  
   #a <-  getURL(linkpl2)
   temp = tempfile()
   test_url(link = linkpl2, output = temp)
   
   
   # run only if downloaded file is valid
   if(!is.na(file.size(temp)) & (file.size(temp) > 0)) {
   
   a = readLines(temp)
   a = paste(a, sep="", collapse="") 
  
  b <- strsplit(a, "Decoded synops since")
  
  b1 <- lapply(b, function(x) substr(x, 1, 400))
  b1[[1]] <- b1[[1]][-1] # header
  
  b21 <- unlist(lapply(gregexpr('Lat=', b1[[1]], fixed = TRUE), function(x) x[1]))
  
  pattern <- paste0(" (", gsub(x = country, pattern = "+", replacement = " ", fixed = TRUE))
  b22 <- unlist(lapply(gregexpr(pattern = pattern, b1[[1]], fixed = TRUE), function(x) x[1]))
  
  b1 <- data.frame(str = b1[[1]], start = b21, stop = b22, stringsAsFactors = FALSE)
  
  res <- substr(b1$str, b1$start, b1$stop)
  
  station_names <- unlist(lapply(strsplit(res, " - "), function(x) x[length(x)]))
  
  
  res <- gsub(x = res, pattern = ", CAPTION, '", replacement = '', fixed = TRUE)
  res <- gsub(x = res, pattern = " m'", replacement = ' ', fixed = TRUE)
  res <- gsub(x = res, pattern = " - ", replacement = ' ', fixed = TRUE)
  res <- gsub(x = res, pattern = "Lat=", replacement = '', fixed = TRUE)
  res <- gsub(x = res, pattern = "Lon=", replacement = ' ', fixed = TRUE)
  res <- gsub(x = res, pattern = "Alt=", replacement = ' ', fixed = TRUE)
  
  res <- suppressWarnings(do.call("rbind", strsplit(res, " ")))
  
  res1 <- res[, c(1, 3, 5:7)]
  
  lat <- as.numeric(substr(res1[, 1], 1, 2)) +
    (as.numeric(substr(res1[,1], 4, 5))/100) * 1.6667
  
  lon_hemisphere <-  gsub("[0-9]", "\\1", res1[, 2])
  lon_hemisphere <-  gsub("-", "", lon_hemisphere)
  lon_hemisphere <- ifelse(lon_hemisphere == "W", -1, 1)
  
  lat_hemisphere <-  gsub("[0-9]", "\\1", res1[, 1])
  lat_hemisphere <-  gsub("-", "", lat_hemisphere)
  lat_hemisphere <- ifelse(lat_hemisphere == "S", -1, 1)
  
  lon <- as.numeric(substr(res1[, 2], 1, 3)) + (as.numeric(substr(res1[, 2], 5, 6)) /
                                                  100) * 1.6667
  lon <- lon * lon_hemisphere
  
  lat <- as.numeric(substr(res1[, 1], 1, 2)) + (as.numeric(substr(res1[, 1], 4, 5)) /
                                                  100) * 1.6667
  lat <- lat * lat_hemisphere
  
  res <- data.frame(wmo_id = res1[, 4], station_names = station_names,
                    lon = lon, lat = lat, alt = as.numeric(res1[, 3]))
  
   } else {
     res = NULL
     cat(paste("Wrong name of a country. Please check countries names at 
         https://ogimet.com/display_stations.php?lang=en&tipo=AND&isyn=&oaci=&nombre=&estado=&Send=Send"))
   } # end of checking problems with internet connection:
   
  
if (!is.null(res)) {
  
  if(add_map == TRUE){
    if (!requireNamespace("maps", quietly = TRUE)){
      stop("package maps required, please install it first")
    }
    # plot labels a little bit higher...
    addfactor <- as.numeric(diff(stats::quantile(res$lat, na.rm = TRUE, c(0.48, 0.51))))
    addfactor <- ifelse(addfactor > 0.2, 0.2, addfactor)
    addfactor <- ifelse(addfactor < 0.05, 0.05, addfactor)
    
    graphics::plot(res$lon, res$lat, col='red', pch=19, xlab = 'longitude', ylab = 'latitude')
    graphics::text(res$lon, res$lat + addfactor, labels = res$station_names,
                   col = 'grey70', cex = 0.6)
    maps::map(add = TRUE)
  }
  
} # end of checking if res is NULL
  
  return(res)
  
}

