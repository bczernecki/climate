#' Sounding data
#'
#' Downloading the mea (i.e., measurements of the vertical profile of atmosphere) sounding data
#'
#' @param wmo_id international WMO station code (World Meteorological Organization ID); For Polish stations: Łeba - 12120, Legionowo - 12374, Wrocław- 12425
#' @param yy year - single number
#' @param mm month - single number denoting month
#' @param dd day - single number denoting day
#' @param hh hour - single number denoting initial hour of sounding; for most stations this measurement is done twice a day (i.e. at 12 and 00 UTC), sporadically 4 times a day
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
#'  \item SKNT - Wind speed (knots)
#'  \item THTA = (K)
#'  \item THTE = (K)
#'  \item THTV = (K)
#'  }
#'  The second list contains metadata and calculated thermodynamic / atmospheric instability indices
#'
#' @return A list of 2 data.frames where first data frame represents parameters of upper parts o with columns describing the meteorogical parameters (e.g. temperature, air pressure) where each row represent a measurement,
#' depending on the height. Secound data.frame present a description of the conditions under which the sounding was carried out. 
#'
#' @source http://weather.uwyo.edu/upperair/sounding.html
#' @export
#'
#' @examples 
#' \donttest{
#' # generate the date to download randomly:
#' 
#'   profile = sounding_wyoming(wmo_id = 12120, 
#'                              yy = sample(2000:2019,1),
#'                              mm = sample(1:12,1), 
#'                              dd = sample(1:20,1), 
#'                              hh = 0)
#'   head(profile)
#'   plot(profile[[1]]$HGHT, profile[[1]]$PRES, type = 'l')
#' }
#'

sounding_wyoming <- function(wmo_id, yy, mm, dd, hh){

  if (length(yy)!=1 || length(mm)!=1 || length(dd)!=1 || length(hh)!=1) {
    stop("The function supports downloading data for a given day. Please change arguments yy, mm, dd, hh to single values")
  }
  
  if (length(wmo_id)!=1) {
    stop("The function supports downloading data for one station at the time. Please change the `wmo_id` argument to a single value")
  }
  
  mm <- formatC(mm, width = 2, format = "d", flag = "0")
  dd <- formatC(dd, width = 2, format = "d", flag = "0")
  hh <- formatC(hh, width = 2, format = "d", flag = "0")
 
  url <- paste0("http://weather.uwyo.edu/cgi-bin/sounding?region=europe&TYPE=TEXT%3ALIST&YEAR=",
                yy, "&MONTH=", mm, "&FROM=", dd, hh, "&TO=", dd, hh, "&STNM=", wmo_id)

  temp <- tempfile()
  test_url(url, temp)
  
  # run only if downloaded file is valid
  df = NULL
  if(!is.na(file.size(temp)) & (file.size(temp) > 800)) { 

    txt <- read.fwf(file = temp, widths = 1000)
    sects <- grep(pattern = "PRE>", x = txt$V1)
    if (length(sects) == 0){
      stop("HTTP status was '503 Service Unavailable'. Have you provided a correct station id?
      Please check wmo_id numbers at:
      http://weather.uwyo.edu/upperair/sounding.html")
    }
    df <- read.fwf(file = temp, skip = sects[1] + 4, widths = rep(7, 11),
                     n = (sects[2] - (sects[1] + 5)))
    
    colnames(df) <- c("PRES", "HGHT", "TEMP", "DWPT", "RELH",
                      "MIXR", "DRCT", "SKNT", "THTA", "THTE", "THTV")
  
    txt <- read.fwf(file = temp, skip = sects[2] + 1, widths = 1000,
                      n = (sects[3] - (sects[2] + 2)), stringsAsFactors = FALSE)$V1
    df2 <- as.data.frame(matrix(data = unlist(strsplit(txt, split = ": ")), ncol = 2, byrow = TRUE))
    colnames(df2) <- c("parameter"," value")
    df <- list(df, df2)
  
    } else { # end of checking file size / problems with internet connection
     cat(paste0("Service not working or wmo_id or date not correct. Check url:\n", url)) 
    }
      
  
  unlink(temp)

  return(df)
}

