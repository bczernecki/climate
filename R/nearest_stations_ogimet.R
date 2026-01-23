#' List of nearby synop stations for a defined geographical location
#'
#' Returns a data frame of meteorological stations with their coordinates and distance from a given location based on the ogimet webpage. 
#' The returned list is valid only for a given day. 
#'
#' @param country country name; It is possible to provide more than one country combined into a vector
#' @param date optionally, a day when measurements were done in all available locations; `Sys.Date` used by default
#' @param add_map logical - whether to draw a map for a returned data frame (requires maps/mapdata packages)
#' @param point a vector of two coordinates (longitude, latitude) for a point we want to find nearest stations to (e.g. c(0, 0))
#' @param no_of_stations how many nearest stations will be returned from the given geographical coordinates
#' @param allow_failure logical - whether to proceed or stop on failure. By default set to TRUE (i.e. don't stop on error). For debugging purposes change to FALSE
#' @param ... extra arguments to be provided to the [graphics::plot()] function (only if add_map = TRUE)
#' @export
#' @return A data.frame with number of nearest station according to given point columns describing stations parameters 
#' (e.g. ID station, distance from point in km, geographic coordinates, etc.). Each row represent a measurement,
#' each station which has a measurements on selected date. If `add_map = TRUE` additional map of downloaded data is added. 
#' 
#' @importFrom utils object.size
#'  
#' @examples 
#' \donttest{
#'   nearest_stations_ogimet(country = "United Kingdom", 
#'                           point = c(-2, 50),
#'                           add_map = TRUE, 
#'                           no_of_stations = 50, 
#'                           allow_failure = TRUE,
#'                           main = "Meteo stations in UK")
#' }
#'

nearest_stations_ogimet = function(country = "United Kingdom", 
                                   date = Sys.Date(), 
                                   add_map = FALSE, 
                                   point = c(2, 50), 
                                   no_of_stations = 10,
                                   allow_failure = TRUE, 
                                   ...) {
  
  if (allow_failure) {
    tryCatch(nearest_stations_ogimet_bp(country = gsub(x = country, " ", "+"),
                                      date = date,
                                      add_map = add_map, 
                                      point = point, 
                                      no_of_stations = no_of_stations,
                                      ...
    ), error = function(e){
      # nocov start
      message(paste("Problems with downloading data.",
                    "Run function with argument allow_failure = FALSE",
                    "to see more details"))})
  } else {
    nearest_stations_ogimet_bp(country = gsub(x = country, " ", "+"),
                             date = date,
                             add_map = add_map, 
                             point = point, 
                             no_of_stations = no_of_stations,
                             ...
    )
      # nocov end
  }
}

#' @keywords Internal
#' @noRd
nearest_stations_ogimet_bp = function(country = country,
                                    date = date, 
                                    add_map = add_map, 
                                    point = point, 
                                    no_of_stations = no_of_stations,
                                    ...) {

  if (length(point) > 2 ) {
    stop("Too many points for the distance calculations. Please provide just one point")
  } else if (length(point) < 2) {
    stop("The point needs to have two coordinates. Please change the `point` argument")
  }
  
  if (length(date) != 1) {
    stop("You can check the available nearest stations for one day only. Please provide just one date")
  }
 
  # initalizing empty data frame for storing results:
  result = NULL
  for (number_countries in country) {

  year = format(date, "%Y")
  month = format(date, "%m")
  day = format(date, "%d")
  ndays = 1
  linkpl2 =
    paste0(
      "https://ogimet.com/cgi-bin/gsynres?lang=en&state=",
      gsub(x = number_countries, " ", "+", fixed = TRUE),
      "&osum=no&fmt=html&ord=REV&ano=",
      year,
      "&mes=",
      month,
      "&day=",
      day,
      "&hora=06&ndays=1&Send=send"
    )

  body = httr::GET(linkpl2,
                   httr::add_headers(
                     `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:143.0) Gecko/20100101 Firefox/143.0",
                     `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
                     `Accept-Language` = "pl,en-US;q=0.7,en;q=0.3",
                     `Referer` = "https://ogimet.com/resynops.phtml.en",
                     `Cookie` = "cookieconsent_status=dismiss; ogimet_serverid=huracan|aNaPt|aNaPj"
                   ))
  body = httr::content(body, as = "text", encoding = "UTF-8")
  
  
  # run only if downloaded file is valid
  if (!is.na(body) & (object.size(body) > 500)) {
    a = paste(body, sep = "", collapse = "") 
    b = strsplit(a, "Decoded synops since")
    b1 = lapply(b, function(x) substr(iconv(x, from = 'UTF-8', to = 'ASCII//TRANSLIT'), 1, 400))
    b1[[1]] = b1[[1]][-1] # header
    b21 = unlist(lapply(gregexpr('Lat=', b1[[1]], fixed = TRUE), function(x) x[1]))
    
    pattern = paste0(" (", gsub(x = number_countries, pattern = "+", replacement = " ", fixed = TRUE))
    b22 = unlist(lapply(gregexpr(pattern = pattern, b1[[1]], fixed = TRUE), function(x) x[1]))
    
    b1 = data.frame(str = b1[[1]], start = b21, stop = b22, stringsAsFactors = FALSE)
    
    res = substr(b1$str, b1$start, b1$stop)
    
    station_names = unlist(lapply(strsplit(res, " - "), function(x) x[length(x)]))
    
    res = gsub(x = res, pattern = ", CAPTION, '", replacement = '', fixed = TRUE)
    res = gsub(x = res, pattern = " m'", replacement = ' ', fixed = TRUE)
    res = gsub(x = res, pattern = " - ", replacement = ' ', fixed = TRUE)
    res = gsub(x = res, pattern = "Lat=", replacement = '', fixed = TRUE)
    res = gsub(x = res, pattern = "Lon=", replacement = ' ', fixed = TRUE)
    res = gsub(x = res, pattern = "Alt=", replacement = ' ', fixed = TRUE)
    
    res = suppressWarnings(do.call("rbind", strsplit(res, " ")))
    
    res1 = res[, c(1, 3, 5:7)]
    
    lat = as.numeric(substr(res1[, 1], 1, 2)) +
      (as.numeric(substr(res1[,1], 4, 5))/100) * 1.6667
    
    lon_hemisphere =  gsub("[0-9]", "\\1", res1[, 2])
    lon_hemisphere =  gsub("-", "", lon_hemisphere)
    lon_hemisphere = ifelse(lon_hemisphere == "W", -1, 1)
    
    lat_hemisphere =  gsub("[0-9]", "\\1", res1[, 1])
    lat_hemisphere =  gsub("-", "", lat_hemisphere)
    lat_hemisphere = ifelse(lat_hemisphere == "S", -1, 1)
    
    lon = as.numeric(substr(res1[, 2], 1, 3)) + (as.numeric(substr(res1[, 2], 5, 6)) / 100)*1.6667
    lon = lon*lon_hemisphere
    
    lat = as.numeric(substr(res1[, 1], 1, 2)) + (as.numeric(substr(res1[, 1], 4, 5)) / 100)*1.6667
    lat = lat * lat_hemisphere
    
    res = data.frame(wmo_id = res1[, 4], station_names = station_names,
                      lon = lon, lat = lat, alt = as.numeric(res1[, 3]))
    result = rbind(result,res)
  } else {
    result = NULL
    message(paste("Wrong name of a country. Please check countries names at 
         https://ogimet.com/display_stations.php?lang=en&tipo=AND&isyn=&oaci=&nombre=&estado=&Send=Send"))
  } # end of checking internet connection
  
  } 
  
  if (!is.null(result)) {

  point = as.data.frame(t(point))
  names(point) = c("lon", "lat")
  distmatrix = rbind(point, result[, 3:4])
  distance_points = stats::dist(distmatrix, method = "euclidean")[1:dim(result)[1]]
  result["distance"] = distance_points * 112.196672
  orderd_distance = result[order(result$distance), ]
  result = orderd_distance[1:no_of_stations, ]
  
  # removing rows with all NA records from the obtained dataset;
  # otherwise there might be problems with plotting infinite xlim, ylim, etc..
  result = result[!apply(is.na(result), 1, sum) == ncol(result), ]
  
  # adding units as attributes:
  attr(result[["distance"]], "label") = "km"
  attr(result[["lon"]], "label") = "decimal degrees"
  attr(result[["lat"]], "label") = "decimal degrees"
  attr(result[["alt"]], "label") = "metres AGL"
  
  # nocov start
  if (add_map == TRUE) {
    if (!requireNamespace("maps", quietly = TRUE)) {
      stop("package maps required, please install it first")
    }
    # plot labels a little bit higher...
    addfactor = as.numeric(diff(stats::quantile(result$lat, na.rm = TRUE, c(0.48, 0.51))))
    addfactor = ifelse(addfactor > 0.2, 0.2, addfactor)
    addfactor = ifelse(addfactor < 0.05, 0.05, addfactor)
    
    graphics::plot(
      result$lon,
      result$lat,
      col = "red",
      pch = 19,
      xlab = "longitude",
      ylab = "latitude",
      xlim = (c(min(
        c(result$lon, point$lon)
      ) - 0.5, max(
        c(result$lon, point$lon)
      ) + 0.5)),
      ylim = (c(min(
        c(result$lat, point$lat)
      ) - 0.5, max(
        c(result$lat, point$lat)
      ) + 0.5)), ...
    )
    graphics::points(
      x = point[1],
      y = point[2],
      col = "blue",
      pch = 19,
      cex = 1
    )
    graphics::text(
      result$lon,
      result$lat + addfactor,
      labels = result$station_names,
      col = "grey70",
      cex = 0.6
    )
    maps::map(add = TRUE)
    # nocov end
    
  }
  
  } # end of checking whether result is null
  
  return(result)
}
