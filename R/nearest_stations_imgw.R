#' List of nearby meteorological or hydrological IMGW-PIB stations in Poland
#'
#' Returns a data frame of meteorological or hydrological stations with their coordinates in particular year. 
#' The returned object is valid only for a given year and type of stations (e.g. "synop", "climate" or "precip"). If `add_map = TRUE` additional map of downloaded data is added. 
#'
#' @param type data name; "meteo" (default), "hydro" 
#' @param rank rank of the stations: "synop" (default), "climate", or "precip"; Only valid if type = "meteo"
#' @param year select year for searching nearest station
#' @param add_map logical - whether to draw a map for a returned data frame (requires maps/mapdata packages)
#' @param point a vector of two coordinates (longitude, latitude) for a point we want to find nearest stations to (e.g. c(15, 53)); If not provided calculated as a mean longitude and latitude for the entire dataset
#' @param no_of_stations how many nearest stations will be returned from the given geographical coordinates. 50 used by default
#' @param ... extra arguments to be provided to the [graphics::plot()] function (only if add_map = TRUE)
#' @export
#' @return A data.frame with a list of nearest stations. Each row represents metadata for station which collected measurements in a given year. Particular columns contain stations metadata (e.g. station ID, geographical coordinates, official name, distance in kilometers from a given coordinates). 
#'  
#' @examples 
#' \donttest{
#'   df =  nearest_stations_imgw(type = "meteo", 
#'   rank = "synop",
#'   year = 2018,
#'   point = c(17, 52),
#'   add_map = TRUE, 
#'   no_of_stations = 4)
#' }
#'

nearest_stations_imgw = function(type = "meteo",
                                 rank = "synop",
                                 year = 2018,
                                 add_map = TRUE,
                                 point = NULL,
                                 no_of_stations = 50,
                                 ...) {
  if (length(point) > 2) {
    stop(paste("Too many points for the distance calculations.",
               "Please provide just one pair of coordinates (e.g. point = c(17,53))"))
  } else if (length(point) < 2) {
    msg = paste("The point argument should have two coordinates.",
                 "We will provide nearest stations for mean location of all available stations.",
                 "To change it please change the `point` argument c(LON,LAT)",
                 sep = "\n")
    message(msg)
    Sys.sleep(2) # display message for 2 sec
  }
  
  if (!is.null(point) && point[1] > 180) {
    stop("x should be longitude")
  } else if (!is.null(point) && point[2] > 90) {
    stop("y should be latitude")
  }

  if (max(year) >= as.integer(substr(Sys.Date(), 1, 4)) - 1) {
    message("Data cannot be provided for this repository. Please check the available records at: \n
            https://danepubliczne.imgw.pl/data/dane_pomiarowo_obserwacyjne/")
    Sys.sleep(2)
  }
  
  if (type == "meteo") {
    result = unique(meteo_imgw_monthly(rank = rank, year = year, coords = TRUE)[, c(2:5)])
  } else if (type == "hydro") {
    result = unique(hydro_imgw_annual(year = year, coords = TRUE)[, c(1:4)])
  } else {
    stop("You've provided wrong type argument; please use: \"meteo\", or \"hydro\"")
  }

  if (dim(result)[1] == 0) {
    stop("Probably there is no data in the downloaded object. Please check available records:  
        https://danepubliczne.imgw.pl/data/dane_pomiarowo_obserwacyjne/")
  }
  
  if (is.null(point)) {
  # workaround for different column names:
    if ("LON" %in% colnames(result))
      point = c(round(mean(result$LON, na.rm = TRUE), 2),
                round(mean(result$LAT, na.rm = TRUE), 2))
    if ("X" %in% colnames(result))
      point = c(round(mean(result$X, na.rm = TRUE), 2),
                round(mean(result$Y, na.rm = TRUE), 2))
  }
  
  dist_vec = double(nrow(result))
  for (i in seq_along(dist_vec)) {
    dist_vec[i] = spheroid_dist(point, c(result$X[i], result$Y[i]))
  }
  
  result["distance"] = round(dist_vec, 3)
  result = result[order(result$distance), ]
  result = result[1:no_of_stations, ]
  

  # removing rows with all NA records from the obtained dataset;
  # otherwise there might be problems with plotting infinite xlim, ylim, etc..
  result = result[!apply(is.na(result), 1, sum) == ncol(result), ]
  
  # adding units as attributes:
  attr(result[["distance"]], "label") = "km"
  attr(result[["X"]], "label") = "decimal degrees"
  attr(result[["Y"]], "label") = "decimal degrees"

  if (add_map == TRUE) {
    if (!requireNamespace("maps", quietly = TRUE)) {
      stop("package maps required, please install it first")
    }
    # plot a little bit more:
    addfactor = stats::quantile(result$Y, c(0.48, 0.51), na.rm = TRUE) #lat Y
    addfactor = as.numeric(diff(addfactor))
    addfactor = ifelse(addfactor > 0.2, 0.2, addfactor)
    addfactor = ifelse(addfactor < 0.05, 0.05, addfactor)
    
    graphics::plot(
      result$X,
      result$Y,
      col = "red",
      pch = 19,
      xlab = "longitude",
      ylab = "latitude",
      xlim = c(min(c(result[["X"]], point[1]), na.rm = TRUE) - 1,
               max(c(result[["X"]], point[1]), na.rm = TRUE) + 1),
      ylim = c(min(c(result[["Y"]], point[2]), na.rm = TRUE) - 1,
               max(c(result[["Y"]], point[2]), na.rm = TRUE) + 1),
      ...
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
  
  if (length(year) > 1) {
    message(paste0("Please provide only one year. For more years station's metadata",
                   "may change (name, location or station may stop collecting data)"))
  }
  return(result)
}
