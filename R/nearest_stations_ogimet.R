#' List of nearby synop stations for a defined geographical location
#'
#' Returns a data frame of meteorological stations with their coordinates and distance from a given location based on the ogimet webpage. 
#' The returned list is valid only for a given day. 
#'
#' @param country country name; for more than two words they need to be seperated with a plus character (e.g. "United+Kingdom")
#' @param date optionally, a day when measurements were done in all available locations; current Sys.Date used by default
#' @param add_map logical - whether to draw a map for a returned data frame (requires maps/mapdata packages)
#' @param point a vector of two coordinates (longitude, latitude) for a point we want to find nearest stations to (e.g. c(0, 0))
#' @param nearest logical vector indicating whether to look only for the nearest stations or to all available stations in a country
#' @param numbers_station how many nearest stations will be returned from the given geographical coordinates
#' @importFrom RCurl getURL
#' @importFrom XML readHTMLTable
#' @export
#'
#' @examples \donttest{
#'   nearest_stations_ogimet(country = "United+Kingdom", point = c(10,50), add_map = T, numbers_station = 60)
#' }
#'
<<<<<<< HEAD
nearest_stations_ogimet <- function(country = "United+Kingdom", date = Sys.Date(), add_map = FALSE, nearest = TRUE, point = c(50, 0), numbers_station = 1){
=======
nearest_stations_ogimet <- function(country = "United+Kingdom", date = Sys.Date(), add_map = FALSE, point = c(0, 0), numbers_station = 1){
>>>>>>> 47133b4ebd3d02facad980c26b00ca9ba84c6149
  
  options(RCurlOptions = list(ssl.verifypeer = FALSE)) # required on windows for RCurl
  
  pt <- point
  
  # initalizing empty data frame for storing results:
  
  year <- format(date, "%Y")
  month <- format(date, "%m")
  day <- format(date, "%d")
  ndays <- 1
  linkpl2 <-
    paste0(
      "http://ogimet.com/cgi-bin/gsynres?lang=en&state=",
      country,
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
  
<<<<<<< HEAD
  if(nearest == TRUE){
    point <- as.data.frame(t(point))
    names(point) <-  c("lon", "lat")
    distmatrix <-  rbind(point,res[, 3:4])
    distance_points <-  stats::dist(distmatrix, method = "euclidean")[1:dim(res)[1]]
    res["distance [km]"] <-  distance_points * 112.196672
    orderd_distance <-  res[order(res$distance), ]
    res <-  orderd_distance[1:numbers_station, ]
  }
=======
 
  point=as.data.frame(t(point))
  names(point) = c("lon", "lat")
  distmatrix = rbind(point,res[, 3:4])
  distance_points = stats::dist(distmatrix, method = "euclidean")[1:dim(res)[1]]
  res["distance [km]"] = distance_points * 112.196672
  orderd_distance = res[order(res$distance), ]
  res = orderd_distance[1:numbers_station, ]

>>>>>>> 47133b4ebd3d02facad980c26b00ca9ba84c6149
  
  if(add_map == TRUE){
    if (!requireNamespace("maps", quietly = TRUE)){
      stop("package maps required, please install it first")
    }
    # plot labels a little bit higher...
    addfactor <- as.numeric(diff(stats::quantile(res$lat, na.rm = TRUE, c(0.48, 0.51))))
    addfactor <- ifelse(addfactor > 0.2, 0.2, addfactor)
    addfactor <- ifelse(addfactor < 0.05, 0.05, addfactor)
    
    graphics::plot(res$lon, res$lat, col='red', pch=19, xlab = 'longitude', ylab = 'latitude')
    graphics::points(x= pt[1], y= pt[2], col='blue', pch=19, cex=1)
    graphics::text(res$lon, res$lat + addfactor, labels = res$station_names,
                   col = 'grey70', cex = 0.6)
    maps::map(add = TRUE)
    
  }
  
  
  return(res)
}

