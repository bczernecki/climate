#' Download file in a graceful way
#'
#' Function for downloading & testing url/internet connection according to CRAN policy
#' Example solution strongly based on 
#' https://community.rstudio.com/t/internet-resources-should-fail-gracefully/49199/12
#' as suggested by kvasilopoulos
#' 
#' @param link character vector with URL to check
#' @param output character vector for output file name
#' @param quiet logical vector (TRUE or FALSE) to be passed to curl_download function. 
#' FALSE by default
#'
#' @importFrom curl curl_download 
#' @importFrom curl has_internet
#' @import httr
#' @return No return value, called for side effects
#' @export
#'
#' @examples
#' \donttest{
#'  link = "https://www1.ncdc.noaa.gov/pub/data/noaa/2019/123300-99999-2019.gz"
#'  output = tempfile()
#'  test_url(link = link, output = output)
#' }
#'

test_url = function(link, output, quiet = FALSE) {
  try_GET = function(x, ...) {
    tryCatch(
      curl::curl_download(url = link, destfile = output, mode = "wb", quiet = quiet, ...),
      error = function(e) conditionMessage(e),
      warning = function(w) conditionMessage(w)
    )
  }
  is_response = function(x) {
    class(x) == "response"
  }
  
  # First check internet connection
  if (!curl::has_internet()) {
    message("No internet connection! \n")
    return(invisible(NULL))
  }
  # Then try for timeout problems
  resp = try_GET(link)
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
