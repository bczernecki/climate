#' Scrapping hourly meteorological (Synop) data from the Ogimet webpage
#'
#' Downloading hourly (meteorological) data from the Synop stations available in the https://www.ogimet.com/ repository
#'
#' @param date start and finish of date (e.g., date = c("2018-05-01","2018-07-01") ); By default last 30 days are taken
#' @param station WMO ID of meteorological station(s). Character or numeric vector
#' @param precip_split whether to split precipitation fields into 6/12/24h; default: TRUE
#' @param allow_failure logical - whether to proceed or stop on failure. By default set to TRUE (i.e. don't stop on error). 
#' For debugging purposes change to FALSE
#' @importFrom utils object.size setTxtProgressBar txtProgressBar
#' 
#' @export
#' 
#' @keywords internal
#' @returns data.frame with historical meteorological data for hourly time interval
#'
#' @examples 
#' \donttest{
#'   # downloading hourly data for Poznan-Lawica, Poland for (default) last 30 days:
#'   poznan = ogimet_hourly(station = 12330)
#' }
#'

ogimet_hourly = function(date = c(Sys.Date() - 30, Sys.Date()), 
                         station = 12330,
                         precip_split = TRUE,
                         allow_failure = TRUE) {
  
  if (allow_failure) {
    tryCatch(ogimet_hourly_bp(date = date,
                              station = station, 
                              precip_split = precip_split),
             error = function(e){
               message(paste("Problems with downloading data.",
                             "Run function with argument allow_failure = FALSE",
                             "to see more details"))})
  } else {
    ogimet_hourly_bp(date = date,
                     station = station, 
                     precip_split = precip_split)
  }
}

#' @keywords Internal
#' @noRd
ogimet_hourly_bp = function(date = date,
                            station = station,
                            precip_split = precip_split) {
  
  dates = seq.Date(min(as.Date(date)), max(as.Date(date)), by = "1 month") - 1
  dates = unique(c(dates, as.Date(max(date))))
  diff_dates = diff(dates)

  # initalizing empty data frame for storing results:
  data_station =
    data.frame(
      "Date" = character(),
      "hour" = character(),
      "UTC" = character(),
      "TC" = character(),
      "TdC" = character(),
      "TmaxC" = character(),
      "TminC" = character(),
      "ddd" = character(),
      "ffkmh" = character(),
      "Gustkmh" = character(),
      "P0hPa" = character(),
      "PseahPa" = character(),
      "PTnd" = character(),
      "Precmm" = character(),
      "Nt" = character(),
      "Nh" = character(),
      "HKm" = character(),
      "InsoD1" = character(),
      "Viskm" = character(),
      "Snowcm" = character(),
      "WW" = character(),
      "W1" = character(),
      "W2" = character(),
      stringsAsFactors = FALSE
    )

  for (station_nr in station) {
    message(paste("station: ", station_nr))
    # adding progress bar if at least 3 iterations are needed
    if (length(dates)*length(station) >= 3 ) pb = txtProgressBar(min = 0, max = length(dates)*length(station) - 1, style = 3)
    
    for (i in length(dates):1) {
      if (length(dates) >= 2 && i == length(dates)) {
        msg = "\n INFO: Please note that the Ogimet has recently limited number of queries that are accepted
        \r by the server from a single IP address. Therefore, downloading more than approx. 1 day of data
        \r for a single station requires 20 seconds pause between subsequent queries and
        \r may take a while. Thank you for your patience."
        message(trimws(msg))
      }
      
      if (i != length(dates)) {
        Sys.sleep(20) # to avoid ogimet server overload
      }
      
      if (length(dates) >= 3 ) paste(setTxtProgressBar(pb, abs(length(dates) * length(station) - i)), "\n")
      
      year = format(dates[i], "%Y")
      month = format(dates[i], "%m")
      day = format(dates[i], "%d")
      ndays = as.numeric(diff_dates[i - 1])
      ndays = ifelse(ndays <= 0, 1, ndays)
      ndays = sprintf("%02d", ndays)
      
      linkpl2 = paste("https://ogimet.com/cgi-bin/gsynres?ind=",
                      station_nr,
                      "&lang=en&decoded=yes&ndays=",
                      ndays,
                      "&ano=",
                      year,
                      "&mes=",
                      month,
                      "&day=",
                      day,
                      "&hora=06",
                      sep = "")
      
      if (month == "01") {
        linkpl2 = paste("http://ogimet.com/cgi-bin/gsynres?ind=",
                        station_nr,
                        "&lang=en&decoded=yes&ndays=32&ano=",
                        year,
                        "&mes=02&day=1&hora=06",
                        sep = "")
      }
      
      body = httr::GET(linkpl2,
                    httr::add_headers(
                      `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:143.0) Gecko/20100101 Firefox/143.0",
                      `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
                      `Accept-Language` = "pl,en-US;q=0.7,en;q=0.3",
                      `Referer` = "https://ogimet.com/resynops.phtml.en",
                      `Cookie` = "cookieconsent_status=dismiss; ogimet_serverid=huracan|aNaPt|aNaPj"
                      ))
      body = httr::content(body, as = "text", encoding = "UTF-8")
      
      # run only if downloaded object is valid
      if (object.size(body) > 1000) {
        b = tryCatch(
          parse_html_table(body, table_start_pattern = '<TABLE align="center" border=0[^>]*>'),
          error = function(e) NULL
        )

        if (is.null(b) || nrow(b) == 0) {
          warning(paste0("Wrong station ID: ", station_nr,
                         " You can check station ID at https://ogimet.com/display_stations.php?lang=en&tipo=AND&isyn=&oaci=&nombre=&estado=&Send=Send"))
          return(data_station)
        }

        # parse_html_table returns the header row without so need to move names 1 position to the right:
        if (!any(names(b) %in% c("hour", "UTC"))) {
          names(b)[2:ncol(b)] = names(b)[1:(ncol(b) - 1)]
          names(b)[2] = "hour"
        }

        b["station_ID"] = station_nr

        coordinates = sapply(c("Latitude", "Longitude"), function(lab) {
          trimws(regmatches(body,
                            regexec(paste0(lab, ":\\s*<FONT[^>]*>([^<]*)"),
                                    body, ignore.case = TRUE)
          )[[1]][2])
        })
        b["Lon"] = round(get_coord_from_string(coordinates["Longitude"], "Longitude"), digits = 4)
        b["Lat"] = round(get_coord_from_string(coordinates["Latitude"], "Latitude"), digits = 4)

        data_station = data.table::rbindlist(list(data_station, b), fill = TRUE, use.names = TRUE)

      } # end of checking for empty files / problems with connection
    } # end of looping for dates
  } # end of looping for stations
  
  if (nrow(data_station) > 0) {
    data_station = unique(data_station)
    # converting character to proper field representation:
    # get rid off "---" standing for missing/blank fields:
    data_station[which(data_station == "--" | data_station == "---" | data_station == "----" | data_station == "-----", arr.ind = TRUE)] = NA
    # changing time..
    data_station$Date = as.POSIXct(paste(data_station$Date, data_station$hour), "%m/%d/%Y %H:%M", tz = 'UTC')
    data_station$hour = NULL
    
    # changing order of columns and removing blank records:
    ord1 = c("station_ID", "Lon", "Lat", "Date", "TC")
    ord1 = c(ord1, setdiff(names(data_station), c("station_ID", "Lon", "Lat", "Date", "TC")))
    data_station = data_station[, ..ord1]
    
    # splitting precipitation into 6-12-24 hours from a default string in the Precmm column:
    if (precip_split) {
      if (all(is.na(data_station$Precmm))) {
        data_station$pr6 = NA
        data_station$pr12 = NA
        data_station$pr24 = NA
      } else {
      data_station$pr6 = precip_split(data_station$Precmm, pattern = "/6")
      data_station$pr12 = precip_split(data_station$Precmm, pattern = "/12")
      data_station$pr24 = precip_split(data_station$Precmm, pattern = "/24")
      }
    }
  
    # clipping to interesting period as we're downloading slightly more than needed:
    data_station = data_station[which(as.Date(data_station$Date) >= as.Date(min(date)) & as.Date(data_station$Date) <= as.Date(max(date))), ]
  
    } # end of checking whether object is empty

  data_station = unique(data_station)
  rownames(data_station) = NULL
  return(data_station)
}
