#' Sounding data
#'
#' Downloading the mea (i.e., measurements of the vertical profile of atmosphere) sounding data
#'
#' @param wmo_id international WMO station code (World Meteorological Organization ID); For Polish stations: Łeba - 12120, Legionowo - 12374, Wrocław- 12425
#' @param yy year - single number
#' @param mm month - single number denoting month
#' @param dd day - single number denoting day
#' @param hh hour - single number denoting initial hour of sounding; for most stations this measurement is done twice a day (i.e. at 12 and 00 UTC), sporadically 4 times a day
#' @importFrom utils download.file read.fwf
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
#' @source http://weather.uwyo.edu/upperair/sounding.html
#' @export
#'
#' @examples \donttest{
#'   sounding <- meteo_sounding(wmo_id = 12120, yy = 2019, mm = 4, dd = 4, hh = 0)
#'   head(sounding)
#'   plot(sounding[[1]]$HGHT, sounding[[1]]$PRES, type = 'l')
#' }
#'

meteo_sounding <- function(wmo_id, yy, mm, dd, hh){

  mm <- formatC(mm, width = 2, format = "d", flag = "0")
  dd <- formatC(dd, width = 2, format = "d", flag = "0")
  hh <- formatC(hh, width = 2, format = "d", flag = "0")

  url <- paste0("http://weather.uwyo.edu/cgi-bin/sounding?region=europe&TYPE=TEXT%3ALIST&YEAR=",
                yy, "&MONTH=", mm, "&FROM=", dd, hh, "&TO=", dd, hh, "&STNM=", wmo_id)

  temp <- tempfile()
  download.file(url, temp)

  txt <- read.fwf(file = temp, widths = 1000)
  sects <- grep(pattern = "PRE>", x = txt$V1)
  df <- read.fwf(file = temp, skip = sects[1] + 4, widths = rep(7, 11),
                 n = (sects[2] - (sects[1] + 5)))
  colnames(df) <- c("PRES", "HGHT", "TEMP", "DWPT", "RELH",
                    "MIXR", "DRCT", "SKNT", "THTA", "THTE", "THTV")

  txt <- read.fwf(file = temp, skip = sects[2] + 1, widths = 1000,
                    n = (sects[3] - (sects[2] + 2)), stringsAsFactors = FALSE)$V1
  df2 <- as.data.frame(matrix(data = unlist(strsplit(txt, split = ": ")), ncol = 2, byrow = TRUE))
  colnames(df2) <- c("parameter"," value")
  df <- list(df, df2)
  unlink(temp)

  return(df)
}

