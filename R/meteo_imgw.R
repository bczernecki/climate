#' Meteorological data from IMGW
#'
#' Downloading hourly, daily, and monthly meteorological data from the SYNOP / CLIMATE / PRECIP stations available in the dane.imgw.pl collection
#'
#' @param interval temporal resolution of the data ("hourly", "daily", "monthly")
#' @param rank rank of the stations: "synop" (default), "climate" or "precip"
#' @param year vector of years (e.g., 1966:2000)
#' @param status leave the columns with measurement and observation statuses (default status = FALSE - i.e. the status columns are deleted)
#' @param coords add coordinates of the station (logical value TRUE or FALSE)
#' @param col_names three types of column names possible: "short" - default, values with shorten names, "full" - full English description, "polish" - original names in the dataset
#' @param ... other parameters that may be passed to the 'shortening' function that shortens column names
#' @param station vector of hydrological stations dane.imgw.pl can be name of station CAPITAL LETTERS(character)
#' It accepts names (characters in CAPITAL LETTERS) or stations' IDs (numeric)
#' @export
#' @return A data.frame with columns describing the meteorological parameters (e.g. temperature, wind speed, precipitation) where each row represent a measurement,
#'  depending on the interval, at a given hour, month or year. If `coords = TRUE` additional two columns with geografic coordinates are added. 
#'  
#' @examples 
#' \donttest{
#'   x <- meteo_imgw("monthly", year = 2018, coords = TRUE)
#'   head(x)
#' }
meteo_imgw <- function(interval, rank = "synop", year, status = FALSE, coords = FALSE, station = NULL, col_names = "short", ...){
  if (interval == "daily"){
    # daily
    calosc <- meteo_imgw_daily(rank = rank, year = year, status = status, coords = coords, station = station, col_names = col_names, ...)
  } else if (interval == "monthly"){
    #monthly
    calosc <- meteo_imgw_monthly(rank = rank, year = year, status = status, coords = coords, station = station, col_names = col_names, ...)
  } else if (interval == "hourly"){
    #hourly
    calosc <- meteo_imgw_hourly(rank = rank, year = year, status = status, coords = coords, station = station, col_names = col_names, ...)
  } else{
    stop("Wrong `interval` value. It should be either 'hourly', 'daily', or 'monthly'.")
  }

  return(calosc)
}
