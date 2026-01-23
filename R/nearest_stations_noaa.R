#' List of nearby SYNOP stations for a defined geographical location
#'
#' Returns a data frame of meteorological stations with their coordinates and distance from a given location based on the noaa website. 
#' The returned list is valid only for a given day. 
#'
#' @param country country name (e.g., "SRI LANKA"). Single entries allowed only.
#' @param date optionally, a day when measurements were done in all available locations; current Sys.Date used by default
#' @param add_map logical - whether to draw a map for a returned data frame (requires maps/mapdata packages)
#' @param point a vector of two coordinates (longitude, latitude) for a point we want to find 
#' nearest stations to (e.g. c(80, 6)). If not provided the query will be based on a mean longitude and latitude among available dataset.
#' @param no_of_stations how many nearest stations will be returned from the given geographical coordinates; default 30
#' @param allow_failure logical - whether to allow or stop on failure. By default set to TRUE. For debugging purposes change to FALSE
#' @importFrom XML readHTMLTable
#' @export
#' @return A data.frame with number of nearest station according to given point columns describing stations parameters 
#' (e.g.  ID station, distance from point, geographic coordinates, etc.) where each row represent a measurement,
#'  each station which has a measurements on selected date. If `add_map = TRUE` additional map of downloaded data is added. 
#'  
#' @examples 
#' \donttest{
#'   nearest_stations_noaa(country = "SRI LANKA", 
#'   point = c(80, 6),
#'   add_map = TRUE, 
#'   no_of_stations = 10)
#'   
#'   uk_stations = nearest_stations_noaa(country = "UNITED KINGDOM", no_of_stations = 100)
#' }
#'

nearest_stations_noaa = function(country,
                                 date = Sys.Date(), 
                                 add_map = TRUE, 
                                 point = NULL, 
                                 no_of_stations = 10, 
                                 allow_failure = TRUE) {
  
  if (allow_failure) {
    tryCatch(nearest_stations_noaa_bp(country = toupper(country),
                                      date = date,
                                      add_map = add_map, 
                                      point = point, 
                                      no_of_stations = no_of_stations
                                      ), error = function(e){
                                        message(paste("Problems with downloading data.",
                                                "Run function with argument allow_failure = FALSE",
                                                "to see the reason"))})
  } else {
    nearest_stations_noaa_bp(country = toupper(country),
                             date = date,
                             add_map = add_map, 
                             point = point, 
                             no_of_stations = no_of_stations
    )
  }
}

#' @keywords Internal
#' @noRd
nearest_stations_noaa_bp = function(country,
                                  date = date, 
                                  add_map = TRUE, point = NULL, 
                                  no_of_stations = 10
                                  ) {
  
  if (missing(country) | is.null(country)) {
    stop("No country provided!")
  }
  
  if (length(point) > 2) {
    stop("Too many points for the distance calculations. Please provide just one point")
  } else if (length(point) < 2) {
    message("The point argument should have two coordinates. \n We will provide nearest stations for mean location. \n To change it please change the `point` argument c(LON,LAT)" )
  }
  
  if (length(date) != 1) {
    stop("You can check the available nearest stations for one day only. Please provide just one date")
  }
  
  linkpl2 = "https://www.ncei.noaa.gov/pub/data/noaa/country-list.txt"
  temp = tempfile()
  test_url(link = linkpl2, output = temp)
  
  # check connection:
  if (!is.na(file.size(temp)) & (file.size(temp) > 800)) { 
  
  a = readLines(temp)
  a = trimws(a, which = "right")
  b = strsplit(a, "          ")
  b1 = do.call(rbind, b)
  colnames(b1) = c("CTRY","countries")
  b1 = as.data.frame(b1[2:dim(b1)[1], ])
  b1$CTRY = as.character(b1$CTRY)
  b1$countries = as.character(b1$countries)
  b2 = read.csv("https://www.ncei.noaa.gov/pub/data/noaa/isd-history.csv")
  stations_noaa = merge(b1, b2)
  stations_noaa["Begin_date"] = as.Date(paste0(substr(stations_noaa[,11], 1, 4), "-",
                                               substr(stations_noaa[,11], 5, 6), "-",
                                               substr(stations_noaa[,11], 7, 8)))
  stations_noaa["End_date"] = as.Date(paste0(substr(stations_noaa[,12], 1, 4), "-",
                                             substr(stations_noaa[,12], 5, 6), "-",
                                             substr(stations_noaa[,12], 7, 8)))
  result = stations_noaa
  
  if (!is.null(country)) {
    result = result[result$countries == country,]
  }

  if (dim(result)[1] == 0) {
    stop("Wrong name of a country. Please check countries names at: 
         https://www.ncei.noaa.gov/pub/data/noaa/country-list.txt")
  } 
  result = result[(result$Begin_date < date & result$End_date < date), ]
  if (dim(result)[1] == 0) {
    stop("Probably there is no data for this date. Please check available records:  
        https://www.ncei.noaa.gov/pub/data/noaa/isd-history.txt")
  }
  if (is.null(point)) {
    point = c(round(mean(result$LON, na.rm = TRUE), 2),round(mean(result$LAT, na.rm = TRUE), 2))
  }
  point = as.data.frame(t(point))
  names(point) = c("LON", "LAT")
  distmatrix = rbind(point,result[, 8:9])
  distance_points = stats::dist(distmatrix, method = "euclidean")[1:dim(result)[1]]
  result["distance"] = distance_points * 112.196672
  orderd_distance = result[order(result$distance), ]
  result = orderd_distance[1:no_of_stations, ]
  
  # removing rows with all NA records from the obtained dataset;
  # otherwise there might be problems with plotting infinite xlim, ylim, etc..
  result = result[!apply(is.na(result), 1, sum) == ncol(result), ]
  
  # adding units as attributes:
  attr(result[["distance"]], "label") = "km"
  attr(result[["LON"]], "label") = "decimal degrees"
  attr(result[["LAT"]], "label") = "decimal degrees"
 
  if (add_map == TRUE) {
    if (!requireNamespace("maps", quietly = TRUE)) {
      stop("package maps required, please install it first")
    }
    # plot labels a little bit higher
    addfactor = as.numeric(diff(stats::quantile(result$LAT, na.rm = TRUE, c(0.48, 0.51))))
    addfactor = ifelse(addfactor > 0.2, 0.2, addfactor)
    addfactor = ifelse(addfactor < 0.05, 0.05, addfactor)
    
    graphics::plot(
      result$LON,
      result$LAT,
      col = "red",
      pch = 19,
      xlab = "longitude",
      ylab = "latitude",
      xlim = (c(min(
        c(result$LON, point$LON)
      ) - 5, max(
        c(result$LON, point$LON)
      ) + 5)),
      ylim = (c(min(
        c(result$LAT, point$LAT)
      ) - 5, max(
        c(result$LAT, point$LAT)
      ) + 5))
    )
    graphics::points(
      x = point[1],
      y = point[2],
      col = "blue",
      pch = 19,
      cex = 1
    )
    if (nrow(result) < 70) {
    graphics::text(
      result$LON,
      result$LAT + addfactor,
      labels = result$USAF,
      col = "grey70",
      cex = 0.6
    )
    }
    maps::map(add = TRUE)
  }

  } else { # end of checking connection
    cat(paste0("Service not working, wrong query or problems with internet connection.\n"))
    result = NULL
  }
  
  return(result)
}
