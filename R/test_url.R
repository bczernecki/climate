#' Download file in a graceful way
#'
#' Function for downloading & testing url/internet connection according to CRAN policy
#' Example solution strongly based on https://community.rstudio.com/t/internet-resources-should-fail-gracefully/49199/12
#' as suggested by kvasilopoulos
#' 
#' @param link character vector with URL to check
#' @param output character vector for output file name
#' @param quiet logical vector (TRUE or FALSE) to be passed to curl_download function. FALSE by default
#'
#' @importFrom curl curl_download 
#' @importFrom curl has_internet
#' @import httr
#' 
#' @export
#'
#' @examples
#' \donttest{
#'  link = "https://www1.ncdc.noaa.gov/pub/data/noaa/2019/123300-99999-2019.gz"
#'  output = basename(link)
#'  test_url(link = link, output = output)
#' }
#'



test_url <- function(link, output, quiet = FALSE) {
  print(link)
  try_GET <- function(x, ...) {
    tryCatch(
      curl::curl_download(url = link, destfile = output, mode = "wb", quiet = quiet, ...),
      error = function(e) conditionMessage(e),
      warning = function(w) conditionMessage(w)
    )
  }
  is_response <- function(x) {
    class(x) == "response"
  }
  
  # First check internet connection
  if (!curl::has_internet()) {
    message("No internet connection! \n")
    return(invisible(NULL))
  }
  # Then try for timeout problems
  resp <- try_GET(link)
  if (!is_response(resp)) {
    message(resp)
    return(invisible(NULL))
  }
  # Then stop if status > 400
  if (httr::http_error(resp)) { 
    message_for_status(resp)
    message(paste0("\nCheck: ", link, " in your browser!\n"))
    return(invisible(NULL))
  }
 
}

# b = gracefully_fail("http://httpbin.org/status/404") # http >400
# #> Not Found (HTTP 404).
# gracefully_fail("http://httpbin.org/delay/11") # Timeout
# #> Timeout was reached: [httpbin.org] Operation timed out after 1000 milliseconds with 0 bytes received
# a = gracefully_fail("http://httpbin.org") #OK
# 
# b = curl_download(url = "http://httpbin.org", destfile = tempfile())
# b = curl_download(url = "http://httpbin.org/status/404", destfile = tempfile())
# 
# url <- "http://www2.census.gov/acs2011_5yr/pums/csv_pus.zip"
# test_url(link = url, output = tempfile())
