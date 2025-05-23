#' Hydrological data from IMGW
#'
#' Downloading daily, and monthly hydrological data from the measurement stations
#' available in the danepubliczne.imgw.pl collection
#'
#' @param interval temporal resolution of the data ("daily" or "monthly")
#' @param year vector of years (e.g., 1966:2000)
#' @param coords add coordinates of the stations (logical value TRUE or FALSE)
#' @param value type of data (can be: state - "H" (default), flow - "Q", or
#' temperature - "T")
#' @param station vector of hydrological stations danepubliczne.imgw.pl;
#' can be given as station name with CAPITAL LETTERS (character)
#' It accepts either names (characters in CAPITAL LETTERS) or stations' IDs (numeric)
#' @param col_names three types of column names possible: "short" - default,
#' values with shorten names, "full" - full English description,
#' "polish" - original names in the dataset
#' @param ... other parameters that may be passed to the 'shortening'
#' function that shortens column names
#' @export
#' @return A data.frame with columns describing the hydrological parameters
#' (e.g. flow, water level) where each row represent a measurement,
#' depending on the interval, at a given hour, month or year.
#' If `coords = TRUE` additional two columns with geographic coordinates are added.
#' @examples
#' \donttest{
#'   x = hydro_imgw("monthly", year = 1999)
#'   head(x)
#' }
hydro_imgw = function(interval,
                      year,
                      coords = FALSE,
                      value = "H",
                      station = NULL,
                      col_names = "short", ...) {

  if (interval == "daily") {
    # dobowe
    calosc = hydro_imgw_daily(year = year, coords = coords, station = station, col_names = col_names, ...)
  } else if (interval == "monthly") {
    # miesieczne
    calosc = hydro_imgw_monthly(year = year,
                                coords = coords,
                                station = station,
                                col_names = col_names, ...)
  } else{
    stop("Wrong `interval` value. It should be either 'daily' or 'monthly'", call. = FALSE)
  }
  return(calosc)
}
