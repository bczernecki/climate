#' Sounding data
#'
#' Downloading the measurements of the vertical profile of atmosphere (also known as sounding data). 
#' Data can be retrieved using TEMP and BUFR sounding formatting. By default automatic detection of input format is used.
#'
#' @param wmo_id international WMO station code (World Meteorological Organization ID); For Polish stations: Leba - 12120, Legionowo - 12374, Wrocław - 12425
#' @param yy year - calendar year
#' @param mm month - calendar month
#' @param dd day - calendar day
#' @param hh hour - hour of sounding; for most stations measurements are performed twice a day (i.e. at 12 and 00 UTC), sporadically 4 times a day
#' @param min minute - minute of sounding; Default 00; Other values applies only to BUFR soundings. 
#' @param source - optional; `AUTODETECT`, `BUFR` (raw; most detailed) or `TEMP` (FM-35) input format. By default "AUTODETECT" is used which means that the function will try to detect the format automatically. If a station use only a specific format that is known you can specify it explicitly.
#' @param allow_failure logical - whether to proceed or stop on failure. By default set to TRUE (i.e. don't stop on error). For debugging purposes change to FALSE
#' @importFrom utils read.fwf
#' @return Returns two lists with values described at: weather.uwyo.edu ; The first list contains:
#' \enumerate{
#'  \item PRES - Pressure (hPa)
#'  \item HGHT - Height (metres)
#'  \item TEMP - Temperature (C)
#'  \item DWPT - Dew point (C)
#'  \item RELH - Relative humidity (%)
#'  \item MIXR - Mixing ratio (g/kg)
#'  \item DRCT - Wind direction (deg)
#'  \item SPED - Wind speed (m/s)
#'  \item THTA - (K)
#'  \item THTE - (K)
#'  \item THTV - (K)
#'  }
#'  The second list contains metadata for time of observation and station location
#'
#' @return A list of 2 data.frames where first data frame represents parameters of upper parts o with columns describing the meteorogical parameters (e.g. temperature, air pressure) where each row represent a measurement,
#' depending on the height. Second data.frame presents a description of the conditions under which the sounding was carried out. 
#'
#' @source http://weather.uwyo.edu/upperair/sounding.html
#' @export
#'
#' @examples 
#' \donttest{
#' # download data for Station 45004 starting 1120Z 11 Jul 2021; Kowloon, HONG KONG, CHINA
#' # using AUTODETECT, TEMP and BUFR sounding formats
#' 
#' # autodect input format:
#' sounding_auto = sounding_wyoming(wmo_id = 45004, 
#'                                  yy = 2021, mm = 07, dd = 17, hh = 12)
#' 
#' # temp (fm35) input format:
#' sounding_temp = sounding_wyoming(wmo_id = 45004, 
#'                                  yy = 2021, mm = 07, dd = 17, hh = 12, 
#'                                  source = "TEMP")
#' # bufr input format:  
#' sounding_bufr = sounding_wyoming(wmo_id = 45004, 
#'                                  yy = 2021, mm = 07, dd = 17, hh = 12, min = 00, 
#'                                  source = "BUFR")
#' }
#'

sounding_wyoming = function(wmo_id, 
                            yy, mm, dd, hh, min = 00, 
                            source = "AUTODETECT",
                            allow_failure = TRUE) {
  
  if (allow_failure) {
    tryCatch(sounding_wyoming_bp(wmo_id, 
                                 yy, mm, dd, hh, min, 
                                 source = source), 
           error = function(e){
             message(paste("Problems with downloading data.",
                           "Run function with argument allow_failure = FALSE",
                           "to see more details"))})
} else {
  sounding_wyoming_bp(wmo_id, 
                      yy, mm, dd, hh, min, 
                      source = source)
  }
}

#' @keywords internal
#' @noRd
sounding_wyoming_bp = function(wmo_id,
                               yy, mm, dd, hh, min, 
                               source = source) {

  if (length(yy) != 1 || length(mm) != 1 || length(dd) != 1 || length(hh) != 1) {
    stop("The function supports downloading data for a given day. Please change arguments yy, mm, dd, hh to single values")
  }
  
  if (length(wmo_id) != 1) {
    stop("The function supports downloading data for one station at the time. Please change the `wmo_id` argument to a single value")
  }
  
  mm = formatC(mm, width = 2, format = "d", flag = "0")
  dd = formatC(dd, width = 2, format = "d", flag = "0")
  hh = formatC(hh, width = 2, format = "d", flag = "0")
  min = formatC(min, width = 2, format = "d", flag = "0")

  if (toupper(source) == "BUFR") {
    src = "BUFR"
  } else if (toupper(source) == "TEMP") {
    src = "FM35"
  } else {
    src = "UNKNOWN"
  }
    
  url = paste0("https://weather.uwyo.edu/wsgi/sounding?datetime=", 
               yy, "-", mm, "-", dd, 
               "%20", hh, ":", min, ":00&id=", 
               sprintf("%05d", wmo_id), "&src=", src, "&type=TEXT:LIST")

  temp = tempfile()
  test_url(url, temp)
  
  # run only if downloaded file is valid
  df = NULL
  if (!is.na(file.size(temp)) & (file.size(temp) > 800)) { 

    txt = read.fwf(file = temp, widths = 1000)
    sects = grep(pattern = "PRE>", x = txt$V1)
    if (length(sects) == 0) {
      stop("HTTP status was '503 Service Unavailable'. Have you provided a correct station id?
      Please check wmo_id numbers at:
      http://weather.uwyo.edu/upperair/sounding.html")
    }
    
    df = read.fwf(file = temp, skip = sects[1] + 4, widths = rep(7, 11),
                     n = (sects[2] - (sects[1] + 5)))

    colnames(df) = c("PRES", "HGHT", "TEMP", "DWPT", "RELH",
                     "MIXR", "DRCT", "SKNT", "THTA", "THTE", "THTV")

    ind = grep(pattern = "Observations", txt$V1)
    df2 = data.frame(sounding_metadata = gsub("<.*?>", "", txt$V1[ind:(ind + 1)]), 
                     stringsAsFactors = FALSE)

    df = list(df, df2)
  
  } else { # end of checking file size / problems with internet connection
     message(paste0("Service not working or wmo_id or date not correct. Check url:\n", url)) 
  }

  unlink(temp)
  return(df)
}
