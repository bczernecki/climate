#' Scrapping meteorological (Synop) data from the Ogimet webpage
#'
#' Downloading hourly or daily (meteorological) data from the Synop stations available at https://www.ogimet.com/
#'
#' @param interval 'daily' or 'hourly' dataset to retrieve - given as character
#' @param date start and finish date (e.g., date = c("2018-05-01", "2018-07-01")) - character or Date class object
#' @param coords add geographical coordinates of the station (logical value TRUE or FALSE)
#' @param station WMO ID of meteorological station(s). Character or numeric vector
#' @param precip_split whether to split precipitation fields into 6/12/24h
#' numeric fields (logical value TRUE (default) or FALSE); valid only for hourly time step
#' @importFrom RCurl getURL
#' @importFrom XML readHTMLTable
#' 
#' @export
#' @return A data.frame with columns describing the synoptic parameters (e.g. air temperature, wind speed, cloudines) where each row represent a measurement,
#' depending on the interval, at a given hour or day.  

#' @examples 
#' \donttest{
#'   # downloading data for Poznan-Lawica
#'   poznan <- meteo_ogimet(interval = "daily", date = c("2019-06-01", "2019-07-08"),
#'                    station = 12330, coords = TRUE)
#'   head(poznan)
#' }
#'
meteo_ogimet <- function(interval, date, coords = FALSE, station, precip_split = TRUE){
  if (interval == "daily"){
    # daily
    if (!precip_split){
      warning("The `precip_split` argument is only valid for hourly time step", call. = FALSE)
    }
    all_data <- ogimet_daily(date = date,  coords = coords, station = station)
  } else if (interval == "hourly"){
    #hourly
    all_data <- ogimet_hourly(date = date,  coords = coords, station = station,
                              precip_split = precip_split)
  } else{
    stop("Wrong `interval` value. It should be either 'hourly' or 'daily'")
  }
  return(all_data)
}
