#' Download gently 
#'
#' Internal function for downloading URLs, but with extra checking of internet connection and stopping if url is wrong or no internet access
#' @param url character vector with URL to check
#' @param output character vector with URL to check
#' 
#' @importFrom RCurl getURL
#' @importFrom utils download.file
#' @import httr
#' 
#' @keywords internal
#'
#' @examples
#' \donttest{
#'  link = "https://www1.ncdc.noaa.gov/pub/data/noaa/2019/123300-99999-2019.gz"
#'  output = basename(link)
#'  climate::download_gently(url = link, output = output)
#' }
#'


download_gently = function(url, output){
  
  if(missing(url) | missing(output)) {
    stop(call. = FALSE, "Both arguments (url and output) have to be provided!")
  }
  
  # pomyslec czy rozwiazanie z trycatchem nie bylo by szybszee, bo nie wymaga
  # testowania przed pobraniem
      if (!httr::http_error(url)) {
        download.file(url, output)
      } else {
        stop(call. = FALSE, 
        paste0("\nDownload failed. ",
               "Check your internet connection or validate this url in your browser: ",
               url, "\n"))
      }
      
}

