#' List of nearby meteorological or hydrological stations in Poland for a defined geographical location
#'
#' Returns a data frame of meteorological or hydrological stations with their coordinates and distance from a given location based on the noaa website. 
#' The returned list is valid only for a given day. 
#'
#' @param type data name;"meteo" (default), "hydro" 
#' @param rank rank of the stations: "synop" (default), "climate", or "precip"
#' @param year select year for serching nearest station
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
#'   rank="synop",
#'   year=2018,
#'   point = c(17, 52),
#'   add_map = TRUE, 
#'   no_of_stations = 4)
#' }
#'

nearest_stations_imgw <- function(type="meteo", 
                                  rank="synop",
                                  year=2018,
                                  add_map = TRUE, point = NULL, 
                                  no_of_stations = 5, ...){
  if (length(point)>2) {
    stop("Too many points for the distance calculations. Please provide just one point")
  } else if (length(point)<2) {
    message("The point should have two coordinates. \n We will provide nearest stations for mean location. \n To change it please change the `point` argument c(LON,LAT)" )
  }
  
  
  if (max(year)>=as.integer(substr(Sys.Date(),1,4))-1) {
    message("Data can be not provided to the repository, please check the availablity at: \n
            https://dane.imgw.pl/data/dane_pomiarowo_obserwacyjne/")
  }
  
  if (type=="meteo"){
    result=unique(meteo_imgw_monthly(rank = rank,year = year,coords = T)[,c(2:5)])
  } else if (type=="hydro"){
    result=unique(hydro_imgw_annual(year = year,coords = T)[,c(1:4)])
  } else {
    stop("You provide wrong type of imgw data please provide \"meteo\", or \"hydro\"")
  }

  if (dim(result)[1]==0) {
    stop("Propobly there is no data. Please check available records :  
        https://dane.imgw.pl/data/dane_pomiarowo_obserwacyjne/")
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
      labels = result$station,
      col = "grey70",
      cex = 0.6
    )
    maps::map(add = TRUE)
    
  }
  if (length(year)>1) {
    message("Please provide only one year, for more years station may change names and duplicate")
  }
  return(result)
}

