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
#' link = "https://www.ncei.noaa.gov/pub/data/noaa/2019/123300-99999-2019.gz"
#' output = tempfile()
#' test_url(link = link, output = output)
#' }
#'
test_url= function(link, output, quiet = TRUE) {
  # First check internet connection
  message(link)
  if (!curl::has_internet()) {
    message("No internet connection! \n")
    return(invisible(NULL))
  }

  # Attempt download; on success curl_download returns the destfile path.
  # Only emit a message if the output file was NOT created (error/warning).
  resp = tryCatch(
    curl::curl_download(url = link, destfile = output, mode = "wb", quiet = quiet),
    error = function(e) conditionMessage(e),
    warning = function(w) conditionMessage(w)
  )

  if (!file.exists(output)) {
    message(resp)
  }

  return(invisible(NULL))
}
