#' List of nearby synop stations for a defined geographical location
#'
#' Returns a data frame of meteorological stations with their coordinates and distance from a given location based on the ogimet webpage. 
#' The returned list is valid only for a given day. 
#'
#' @param country country name; for more than two words they need to be seperated with a plus character (e.g., "United+Kingdom")
#' @param date optionally, a day when measurements were done in all available locations; current Sys.Date used by default
#' @param add_map logical - whether to draw a map for a returned data frame (requires maps/mapdata packages)
#' @param point a vector of two coordinates (longitude, latitude) for a point we want to find nearest stations to (e.g. c(0, 0))
#' @param numbers_station how many nearest stations will be returned from the given geographical coordinates
#' @importFrom RCurl getURL
#' @importFrom XML readHTMLTable
#' @export
#'
#' @examples \donttest{
#'   nearest_stations_ogimet(country = "United+Kingdom", point = c(10, 50),
#'      add_map = TRUE, numbers_station = 60)
#' }
#'

nearest_stations_ogimet <- function(country = "United+Kingdom", date = Sys.Date(), add_map = FALSE, point = c(0, 0), numbers_station = 1){

  options(RCurlOptions = list(ssl.verifypeer = FALSE)) # required on windows for RCurl

  if (length(point)>2) {
    stop("To many points to calculating distance. Please choose one point")
  } else if (length(point)<2) {
    stop("Point need to have to coordinates. Please change your selection")
  }
  
  if (length(date)!=1) {
    stop("You can check available nearest stations for one day. Please chenge selection")
    
  }
  pt <- point
  
  # initalizing empty data frame for storing results:
  result=NULL
  for (number_countires in country) {
  #  print(number_countires)

  year <- format(date, "%Y")
  month <- format(date, "%m")
  day <- format(date, "%d")
  ndays <- 1
  linkpl2 <-
    paste0(
      "http://ogimet.com/cgi-bin/gsynres?lang=en&state=",
      number_countires,
      "&osum=no&fmt=html&ord=REV&ano=",
      year,
      "&mes=",
      month,
      "&day=",
      day,
      "&hora=06&ndays=1&Send=send"
    )
  a <-  getURL(linkpl2)
  
  b <- strsplit(a, "Decoded synops since")
  
  b1 <- lapply(b, function(x) substr(x, 1, 400))
  b1[[1]] <- b1[[1]][-1] # header
  
  b21 <- unlist(lapply(gregexpr('Lat=', b1[[1]], fixed = TRUE), function(x) x[1]))
  
  pattern <- paste0(" (", gsub(x = number_countires, pattern = "+", replacement = " ", fixed = TRUE))
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
  
  res1 <- res[,c(1,3,5:7)]
  
  lat <- as.numeric(substr(res1[, 1], 1, 2)) +
    (as.numeric(substr(res1[,1], 4, 5))/100) * 1.6667
  
  lon_hemisphere <-  gsub("[0-9]", "\\1", res1[, 2])
  lon_hemisphere <-  gsub("-", "", lon_hemisphere)
  lon_hemisphere <- ifelse(lon_hemisphere == "W", -1, 1)
  
  lat_hemisphere <-  gsub("[0-9]", "\\1", res1[, 1])
  lat_hemisphere <-  gsub("-", "", lat_hemisphere)
  lat_hemisphere <- ifelse(lat_hemisphere == "S", -1, 1)
  
  lon <- as.numeric(substr(res1[, 2], 1, 3)) + (as.numeric(substr(res1[, 2], 5, 6)) / 100)*1.6667
  lon <- lon*lon_hemisphere
  
  lat <- as.numeric(substr(res1[, 1], 1, 2)) + (as.numeric(substr(res1[, 1], 4, 5)) / 100)*1.6667
  lat <- lat * lat_hemisphere
  
  res <- data.frame(wmo_id = res1[, 4], station_names = station_names,
                    lon = lon, lat = lat, alt = as.numeric(res1[, 3]))
  result=rbind(result,res)
}
  if (dim(result)[1]==0) {
    stop("Wrong name of country, please check station index database at 
         https://ogimet.com/display_stations.php?lang=en&tipo=AND&isyn=&oaci=&nombre=&estado=&Send=Send")
    

  point=as.data.frame(t(point))
  names(point) = c("lon", "lat")
  distmatrix = rbind(point,result[, 3:4])
  distance_points = stats::dist(distmatrix, method = "euclidean")[1:dim(result)[1]]
  result["distance [km]"] = distance_points * 112.196672
  orderd_distance = result[order(result$distance), ]
  result = orderd_distance[1:numbers_station, ]


  
  if(add_map == TRUE){
    if (!requireNamespace("maps", quietly = TRUE)){
      stop("package maps required, please install it first")
    }
    # plot labels a little bit higher...
    addfactor <- as.numeric(diff(stats::quantile(result$lat, na.rm = TRUE, c(0.48, 0.51))))
    addfactor <- ifelse(addfactor > 0.2, 0.2, addfactor)
    addfactor <- ifelse(addfactor < 0.05, 0.05, addfactor)
    
    graphics::plot(result$lon, result$lat, col='red', pch=19, xlab = 'longitude', ylab = 'latitude', 
                   xlim=(c(min(c(result$lon,point$lon))-0.5, max(c(result$lon,point$lon))+0.5)),
                   ylim=(c(min(c(result$lat,point$lon))-0.5, max(c(result$lat,point$lon))+0.5)))
    graphics::points(x= point[1], y= point[2], col='blue', pch=19, cex=1)
    graphics::text(result$lon, result$lat + addfactor, labels = result$station_names,
                   col = 'grey70', cex = 0.6)
    maps::map(add = TRUE)
    
  }
   }
  return(result)
}

