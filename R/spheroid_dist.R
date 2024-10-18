#' Distance between two points on a spheroid
#' 
#' Calculate the distance between two points on the surface of a spheroid
#' using Vincenty's formula. This function can be used when GIS libraries
#' for calculating distance are not available.
#' 
#' @param p1 coordinates of the first point in decimal degrees (LON, LAT)
#' @param p2 coordinates of the second point in decimal degrees (LON, LAT)
#' 
#' @return numerical vector with distance between two locations (in kilometers)
#' @export
#' @examples 
#'  p1 = c(18.633333, 54.366667) # longitude and latitude for Gdansk, PL
#'  p2 = c(17.016667, 54.466667) # longitude and latitude for Slupsk, PL
#'  spheroid_dist(p1, p2)
#'  
spheroid_dist = function(p1, p2) {

  r = 6371009 # mean earth radius in meters
  vec = c(p1, p2) * pi / 180 # convert degrees to radians
  diff_long = vec[3] - vec[1]

  num = (cos(vec[4]) * sin(diff_long))^2 + (cos(vec[2]) * sin(vec[4]) - sin(vec[2]) * cos(vec[4]) * cos(diff_long))^2
  denom = sin(vec[2]) * sin(vec[4]) + cos(vec[2]) * cos(vec[4]) * cos(diff_long)
  d = atan(sqrt(num) / denom)
  d = d * r
  return(d / 1000) # output in km

}
