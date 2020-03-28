#' Splitting precipitation values to 6/12/24 hour periods and converting to numeric
#'
#' Internal function for splitting precipitation field provided by Ogimet and converting string to numerical value
#' @param precip vector of characters with precipitation field from Ogimet
#' @param pattern 6h-12h-24h precipitation pattern to obtain written as: "/6h", "/12h" , "/24h" (see examples)
#' 
#' @keywords internal
#'
#' @examples
#' \donttest{
#'  df <- climate:::ogimet_hourly(station = 12330)
#'  climate:::precip_split(df$Precmm, pattern = "/12") # to get 12h precipitation amounts
#' }
#'
precip_split <- function(precip,  pattern = "/12"){
  b <- strsplit(precip, "h", fixed = TRUE)
  b <- lapply(b, function(x) x[grepl(x, pattern = pattern, fixed = TRUE)])
  b <- unlist(lapply(b, function(x) ifelse(length(x) > 0, gsub(x = x, pattern = pattern, replacement = ""), NA)))
  suppressWarnings(as.numeric(as.character(b)))
}
