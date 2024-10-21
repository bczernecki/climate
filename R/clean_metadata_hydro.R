#' Hydrological metadata cleaning (IMGW-PIB data only)
#'
#' Internal function for hydrological metadata cleaning
#' @param address URL address of the metadata file
#' @param interval temporal interval
#' @importFrom utils read.fwf
#' @keywords internal
#' @noRd

clean_metadata_hydro = function(address, interval) {
  temp = tempfile()

  test_url(link = address, output = temp)
  a = read.csv(temp, header = FALSE, stringsAsFactors = FALSE, 
               fileEncoding = "CP1250", skip = 1, sep = "\t")$V1
  a = gsub(a, pattern = "\\?", replacement = "") 
  a = gsub(x = a, pattern = "'", replacement = "")
  a = trimws(gsub(x = a, pattern = "\\^", replacement = ""))
  a = gsub(a, pattern = "\\s+", replacement = " ")

  if (interval == "monthly") {
    b = list(data.frame(parameters = a[1:10]))
  }
  if (interval == "daily") {
    b = data.frame(parameters = a[1:10])
  }
  return(b)
}
