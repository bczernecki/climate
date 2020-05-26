#' List of nearby meteorological or hydrological stations in Poland for a defined geographical location
#'
#' Returns a data frame of meteorological or hydrological stations with their coordinates and distance from a given location based on the noaa website. 
#' The returned list is valid only for a given day. 
#'
#' @param type data name;"meteo" (default), "hydro" 
#' @param add_map logical - whether to draw a map for a returned data frame (requires maps/mapdata packages)
#' @param point a vector of two coordinates (longitude, latitude) for a point we want to find nearest stations to (e.g. c(80, 6))
#' @param no_of_stations how many nearest stations will be returned from the given geographical coordinates
#' @param ... extra arguments to be provided to the [graphics::plot()] function (only if add_map = TRUE)
#' @importFrom XML readHTMLTable
#' @export
#' @return A data.frame with number of nearest station according to given point columns describing stations parameters (e.g.  ID station, distance from point,geografic coordinates) where each row represent a measurement,
#'  each station which has a measurements. If `add_map = TRUE` additional map of downloaded data is added. 
#'  
#' @examples 
#' \donttest{
#'   nearest_stations_imgw(type = "hydro", 
#'   point = c(17, 52),
#'   add_map = TRUE, 
#'   no_of_stations = 4)
#' }
#'

nearest_stations_imgw <- function(type="meteo", 
                                  #date = Sys.Date(), function work on internal dataset
                                  add_map = TRUE, point = NULL, 
                                  no_of_stations = 5, ...){
  if (length(point)>2) {
    stop("Too many points for the distance calculations. Please provide just one point")
  } else if (length(point)<2) {
    message("The point should have two coordinates. \n We will provide nearest stations for mean location. \n To change it please change the `point` argument c(LON,LAT)" )
  }
  

#  if (length(date)!=1) {
#    stop("You can check the available nearest stations for one day only. Please provide just one date")
#  }
  if (type=="meteo"){
    result=imgw_meteo_stations
  } else if (type=="hydro"){
    result=imgw_hydro_stations
  } else {
    stop("You provide wrong type of imgw data please provide \"meteo\", or \"hydro\"")
  }
# to do przyszłego rozbudowania ewentualnej zamiany
#  if (type=="meteo"){
#    result=meteo_imgw_monthly(year = 2019)
#  } else if (type=="hydro"){
#    result=hydro_imgw_daily(year = 2019)
#  } else {
#    stop("You provide wrong type of imgw data please provide \"meteo\", or \"hydro\"")
#  }

  if (dim(result)[1]==0) {
    stop("Propobly there is no data. Please check available records :  
        https://dane.imgw.pl/data/dane_pomiarowo_obserwacyjne/dane_meteorologiczne/")
  } 
  if (is.null(point)){
    point=c(round(mean(result$LON,na.rm=T),2),round(mean(result$LAT,na.rm=T),2))
  }
  
  point = as.data.frame(t(point))
  names(point) = c("X", "Y")
  distmatrix = rbind(point,result[, 2:3])
  distance_points = stats::dist(distmatrix, method = "euclidean")[1:dim(result)[1]]
  result["distance [km]"] = distance_points * 112.196672
  orderd_distance = result[order(result$distance), ]
  result = orderd_distance[1:no_of_stations, ]
  
  # removing rows with all NA records from the obtained dataset;
  # otherwise there might be problems with plotting infinite xlim, ylim, etc..
  result = result[!apply(is.na(result), 1, sum) == ncol(result),]
  
 
  if(add_map == TRUE){
    if (!requireNamespace("maps", quietly = TRUE)){
      stop("package maps required, please install it first")
    }
    # plot labels a little bit higher...
    addfactor <- as.numeric(diff(stats::quantile(result$Y, na.rm = TRUE, c(0.48, 0.51)))) #lat Y
    addfactor <- ifelse(addfactor > 0.2, 0.2, addfactor)
    addfactor <- ifelse(addfactor < 0.05, 0.05, addfactor)
    
    graphics::plot(
      result$X,
      result$Y,
      col = "red",
      pch = 19,
      xlab = "longitude",
      ylab = "latitude",
      xlim = (c(min(
        c(result$X, point$X)
      ) - 3, max(
        c(result$X, point$X)
      ) + 3)),
      ylim = (c(min(
        c(result$Y, point$Y)
      ) - 3, max(
        c(result$Y, point$Y)
      ) + 3)), ...
    )
    graphics::points(
      x = point[1],
      y = point[2],
      col = "blue",
      pch = 19,
      cex = 1
    )
    graphics::text(
      result$X,
      result$Y + addfactor,
      labels = result$id,
      col = "grey70",
      cex = 0.6
    )
    maps::map(add = TRUE)
    
  }
  if (type=="meteo"){
    message("Depending of ID synoptic stations may be downloaded by meteo_imgw with different rank \n (eg. if station begin with 3 rank should be \"synop\") \n In different dates, stations may have different availibility of data")
  }else{
    message("In different dates, stations may have different availibility of data")
    
  }
  return(result)
}

