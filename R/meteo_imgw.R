#' Meteorological data from the IMGW-PIB official repository
#'
#' Downloading hourly, daily, and monthly meteorological data from the
#' SYNOP / CLIMATE / PRECIP stations, or sub-hourly (10-minute) telemetry data from the
#' automated network, all available in the danepubliczne.imgw.pl collection.
#'
#' @param interval temporal resolution of the data: `"hourly"`, `"daily"`, or `"monthly"`.
#'   Not used when `rank = "telemetry"` (telemetry data are always at 10-minute intervals).
#'   Defaults to `NULL`, which is only valid together with `rank = "telemetry"`.
#' @param rank rank of the stations: `"synop"` (default), `"climate"`, `"precip"`, or `"telemetry"`.
#'   Use `"telemetry"` for the automated IMGW datastore network (data available since 2008).
#' @param year vector of years (e.g., `1966:2000`).
#'   For `rank = "telemetry"` all years must be >= 2008.
#' @param status leave the columns with measurement and observation statuses
#'   (default `FALSE` — status columns are deleted). Not used when `rank = "telemetry"`.
#' @param coords add coordinates of the station (logical value `TRUE` or `FALSE`).
#'   Default `FALSE`.
#' @param col_names column name style: `"short"` (default), `"full"` (English descriptions),
#'   or `"polish"` (original dataset names). Not used when `rank = "telemetry"`.
#' @param station name of meteorological station(s).
#'   For ranks `"synop"`, `"climate"`, `"precip"`: station name(s) in CAPITAL LETTERS.
#'   Please note that station names may change over time — sometimes two names are required,
#'   e.g. `c("POZNAŃ", "POZNAŃ-ŁAWICA")`.
#'   For `rank = "telemetry"`: station name(s) as listed by `stations_meteo_imgw_telemetry()`.
#'   `NULL` (default) downloads all available stations.
#' @param parameters character vector of parameter codes to download.
#'   Only used when `rank = "telemetry"`. `NULL` (default) downloads all available parameters.
#'   Accepted values: `"wd"`, `"t2m"`, `"t0m"`, `"rr_24h"`, `"rr_1h"`, `"rr_10min"`,
#'   `"ws"`, `"ws_max"`, `"gust"`, `"rh"`, `"water_in_snow"`.
#' @param ... other parameters passed to the column-shortening function. Not used when
#'   `rank = "telemetry"`.
#' @export
#' @return A data.frame with meteorological parameters where each row is a measurement.
#'   For ranks `"synop"`, `"climate"`, `"precip"`: measurements at a given hour, day, or month,
#'   depending on `interval`. If `coords = TRUE` two additional coordinate columns are appended.
#'   For `rank = "telemetry"`: a data.table with 10-minute interval observations (not
#'   expert-validated). If `coords = TRUE` columns `name`, `lon`, `lat`, and `alt` are appended.
#' @examples
#' \donttest{
#'   x = meteo_imgw("monthly", year = 2018, coords = TRUE)
#'   head(x)
#'
#'   # Telemetry (10-minute) data from automated stations (available since 2008):
#'   tel = meteo_imgw(rank = "telemetry", year = 2022,
#'                    parameters = "t2m",
#'                    station = "HALA GĄSIENICOWA")
#'   head(tel)
#' }
meteo_imgw = function(interval = NULL,
                      rank = "synop",
                      year,
                      status = FALSE,
                      coords = FALSE,
                      station = NULL,
                      col_names = "short",
                      parameters = NULL,
                      ...) {
  if (rank == "telemetry") {
    return(meteo_imgw_datastore(year       = year,
                                parameters = parameters,
                                stations   = station,
                                coords     = coords))
  }
  if (is.null(interval)) {
    stop("The `interval` argument is required for rank '", rank,
         "'. Use 'hourly', 'daily', or 'monthly'.")
  }
  if (interval == "daily") {
    result = meteo_imgw_daily(rank      = rank,
                              year      = year,
                              status    = status,
                              coords    = coords,
                              station   = station,
                              col_names = col_names, ...)
  } else if (interval == "monthly") {
    result = meteo_imgw_monthly(rank      = rank,
                                year      = year,
                                status    = status,
                                coords    = coords,
                                station   = station,
                                col_names = col_names, ...)
  } else if (interval == "hourly") {
    result = meteo_imgw_hourly(rank      = rank,
                               year      = year,
                               status    = status,
                               coords    = coords,
                               station   = station,
                               col_names = col_names, ...)
  } else {
    stop("Wrong `interval` value. It should be either 'hourly', 'daily', or 'monthly'.")
  }
  return(result)
}
