#' CO2 Mauna Loa (NOAA) dataset
#'
#' Carbon Dioxide (CO2) monthly measurements from Mauna Loa observatory.
#' The source file is available at: ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt with all further details.
#' 
#' Data from March 1958 through April 1974 have been obtained by C. David Keeling
#' of the Scripps Institution of Oceanography (SIO) and were obtained from the
#' Scripps website (scrippsco2.ucsd.edu).
#' 
#' 
#' The "average" column contains the monthly mean CO2 mole fraction determined
#' from daily averages.  The mole fraction of CO2, expressed as parts per million
#' (ppm) is the number of molecules of CO2 in every one million molecules of dried
#' air (water vapor removed).  If there are missing days concentrated either early
#' or late in the month, the monthly mean is corrected to the middle of the month
#' using the average seasonal cycle.  Missing months are denoted by -99.99.
#' The "interpolated" column includes average values from the preceding column
#' and interpolated values where data are missing.  Interpolated values are
#' computed in two steps.  First, we compute for each month the average seasonal
#' cycle in a 7-year window around each monthly value.  In this way the seasonal
#' cycle is allowed to change slowly over time.  We then determine the "trend"
#' value for each month by removing the seasonal cycle; this result is shown in
#' the "trend" column.  Trend values are linearly interpolated for missing months.
#' The interpolated monthly mean is then the sum of the average seasonal cycle
#' value and the trend value for the missing month.
#
#' NOTE: In general, the data presented for the last year are subject to change, 
#' depending on recalibration of the reference gas mixtures used, and other quality
#' control procedures. Occasionally, earlier years may also be changed for the same
#' reasons.  Usually these changes are minor.
#
#' CO2 expressed as a mole fraction in dry air, micromol/mol, abbreviated as ppm
#'
#' @importFrom utils read.table
#' @importFrom utils data
#' @export
#' 
#'
#' @examples \donttest{
#'   #co2 <- meteo_noaa_co2()
#'   #head(co2)
#'   #plot(co2$yy_d, co2$co2_avg, type='l')
#' }
#'

meteo_noaa_co2 <- function(){
  
  base_url = "ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt"
  temp = tempfile()
  test_url(link = base_url, output = temp)
  
  # run only if downloaded file is valid
  co2 = NULL
  if(!is.na(file.size(temp)) & (file.size(temp) > 800)) { 
    
    co2 = read.table(temp, na.strings = "-99.99")
    colnames(co2) = c("yy", "mm", "yy_d","co2_avg", "co2_interp", "co2_seas", "ndays")
    
  } else {
    cat(paste0("Service not working or problems with internet connection. Check url:\n", base_url)) 
  }
    
  unlink(temp)
  return(co2)
}
