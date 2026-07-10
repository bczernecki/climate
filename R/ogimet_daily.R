#' Scrapping daily meteorological (Synop) data from the Ogimet webpage
#'
#' Downloading daily (meteorological) data from the Synop stations available in the https://www.ogimet.com/ repository.
#' The data are processed only if temperature or precipitation fields are present.
#'
#' @param date start and finish of date (e.g., date = c("2018-05-01","2018-07-01") ). By default last 30 days.
#' @param station WMO ID of meteorological station(s). Character or numeric vector
#' @param hour time for which the daily raport is generated. Set default as hour = 6 (i.e. 6 UTC)
#' @param allow_failure logical - whether to proceed or stop on failure. By default set to TRUE (i.e. don't stop on error). For debugging purposes change to FALSE
#' @param ... extra arguments that may be passed from wrapper top-level functions
#' @importFrom utils setTxtProgressBar txtProgressBar object.size
#' 
#' @export
#' @returns data.frame with historical meteorological data for the daily summaries
#'
#' @examples 
#' \donttest{
#'   # downloading daily summaries for last 30 days. station: New York - La Guardia
#'   new_york = ogimet_daily(station = 72503)
#' }
#'

ogimet_daily = function(date = c(Sys.Date() - 30, Sys.Date()),
                        station = NA, 
                        hour = 6, 
                        allow_failure = TRUE, ...) {

  if (allow_failure) {
    tryCatch(ogimet_daily_bp(date = date, station = station, hour = hour), 
             error = function(e){
               message(paste("Problems with downloading data.",
                             "Run function with argument allow_failure = FALSE",
                             "to see more details"))})
  } else {
    ogimet_daily_bp(date = date, station = station, hour = hour)
  }
}

#' @keywords Internal
#' @noRd
ogimet_daily_bp = function(date = date,
                           station = station, 
                           hour = hour) {
  dates = seq.Date(min(as.Date(date)), max(as.Date(date)), by = "1 month") 
  dates = unique(c(dates, as.Date(max(date))))

  # initalizing empty data frame for storing results:
  message(
    paste(
      "Daily raports will be generated for", hour, 
      "UTC each day. Use the >>hour<< argument to change it \n"
    )
  )
  data_station =
    data.frame(
      "Date" = character(),
      "Lon" = character(),
      "Lat" = character(),
      "Alt" = character(),
      "Temperature_Max" = character(),
      "Temperature_Min" = character(),
      "Temperature_Avg" = character(),
      "TdAvg" = character(),
      "Hr.Avg" = character(),
      "App.TAvg" = character(),
      "Wind_Dir." = character(),
      "Wind_Int." = character(),
      "Wind_Gust" = character(),
      "Pres.s.lev" = character(),
      "Prec" = character(),
      "SunD-1" = character(),
      "SnowDep" = character(),
      "TotClOct" = character(),
      "lowClOct" = character(),
      "VisKm" = character(),
      "station_ID" = character(),
      stringsAsFactors = FALSE
    )
  
  for (station_nr in station) {
    message(paste("station:", station_nr))
    
    # adding progress bar if at least 3 iterations are needed
    if (length(dates) * length(station) >= 3) {
      pb = txtProgressBar(min = 0, max = length(dates) * length(station) - 1, style = 3)
    } 

    for (i in length(dates):1) {
      
      if (length(dates) >= 2 && i == length(dates)) {
        msg = "\n INFO: Please note that the Ogimet has recently limited number of queries that are accepted
        \r by the server from a single IP address. Therefore, downloading more than 1 month of data
        \r for a single station requires 20 seconds pause between subsequent queries and
        \r may take a while. Thank you for your patience."
        message(trimws(msg))
      }
      
      if (i != length(dates)) {
        Sys.sleep(20) # to avoid ogimet server overload
      }
      # update progressbar:
      if (length(dates) >= 3) paste(setTxtProgressBar(pb, abs(length(dates)*length(station) - i)), "\n")
      
      year = format(dates[i], "%Y")
      month = format(dates[i], "%m")
      day = format(dates[i], "%d")
      ndays = 32
      hr = "06" # default hour for daily reports
      
      
      if (Sys.Date() == max(date) & as.numeric(format(Sys.time(), "%H")) < 6) {
        # if current time is before 6 UTC, then download data for 00 UTC of the current day"
        month = format(Sys.time() - 3600, "%m", tz = 'UTC')
        day = format(Sys.time() - 3600, "%d", tz = 'UTC')
        hr = format(Sys.time() - 3600, "%H", tz = 'UTC')
      }

      linkpl2 = paste("https://www.ogimet.com/cgi-bin/gsynres?lang=en&ind=", 
                      station_nr, "&ndays=32&ano=", 
                      year, "&mes=", month, "&day=", day, 
                      "&hora=", hr,"&ord=REV&Send=Send", sep = "")
      
      body = httr::GET(linkpl2,
                       httr::add_headers(
                         `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:143.0) Gecko/20100101 Firefox/143.0",
                         `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
                         `Accept-Language` = "pl,en-US;q=0.7,en;q=0.3",
                         `Referer` = "https://ogimet.com/resynops.phtml.en",
                         `Cookie` = "cookieconsent_status=dismiss; ogimet_serverid=huracan|aNaPt|aNaPj"
                       ))
      body = httr::content(body, as = "text", encoding = "UTF-8")

      if (is.na(body) | (object.size(body) < 500)) { 
        message("Problem with downloading data from:", linkpl2, "\n")
        if (exists("data_station")) {
          message("Returning results downloaded up to this point:\n")
          return(data_station)
        }
      } else { # run only if downloaded file is valid

        b = parse_html_table(body, 
                             table_start_pattern = '<TABLE align="center" border=0[^>]*>')
        
        names(b) = gsub("\\s*\\([^)]*\\)", "", names(b))
        names(b) = gsub("%", "", names(b))
        
        b["station_ID"] = station_nr
        # extra check if date is for December and January simultanously
        # e.g. "01/02" "01/01" "12/31" "12/30"
        uniq_mths = sort(unique(unlist(lapply(strsplit(b$Date, "/"), "[[", 1))))
        
        if (sum(uniq_mths %in% c("01", "12")) == 2) {
            mth = unlist(lapply(strsplit(b$Date, "/"), "[[", 1))
            yr = ifelse(mth == "01", as.numeric(year), as.numeric(year) - 1)
            b$Date = as.character(paste0(b$Date, "/", yr))
          } else {
            b$Date = as.character(paste0(b$Date, "/", year))
          }
        
        # coordinates:
          coordinates = sapply(c("Latitude", "Longitude", "Altitude"), function(lab) {
            trimws(regmatches(body, 
                              regexec(paste0(lab, ":\\s*<FONT[^>]*>([^<]*)"), 
                                      body, ignore.case = TRUE)
            )[[1]][2])
          })
          
          b["Lon"] = round(get_coord_from_string(coordinates["Longitude"], "Longitude"), digits = 4)
          b["Lat"] = round(get_coord_from_string(coordinates["Latitude"], "Latitude"), digits = 4)
          b["Alt"] = as.numeric(coordinates["Altitude"])

        # joining with template or previously downloaded data:
          data_station = data.table::rbindlist(list(data_station, b), fill = TRUE)
        
        } # end of checking for empty files / problems with connection
      } # end of looping for dates
    
    Sys.sleep(20)
    } # end of looping for stations
    
    if (nrow(data_station) > 0) {
      
    # changing order of columns and removing blank records:
      # ord1 = c("station_ID", "Lon", "Lat", "Alt", "Date", "TemperatureCAvg")
      # ord1 = c(ord1, setdiff(colnames(data_station), c("station_ID", "Lon", "Lat", "Alt", "Date", "TemperatureCAvg")))
      # data_station = data_station[, ..ord1]
      data.table::setorder(data_station, -Date, station_ID)
    
    # date to as.Date() and clipping to interesting period as we're downloading a bit more than needed:
      data_station$Date = as.Date(as.character(data_station$Date), format = "%m/%d/%Y")
      data_station = data_station[which(data_station$Date >= as.Date(min(date)) & as.Date(data_station$Date) <= as.Date(max(date))), ]
    } # end of checking whether no. of rows > 0 
  
  # removing duplicates:
  data_station =  unique(data_station)
  return(data_station)
}
