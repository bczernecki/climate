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
  a = read.csv(temp, header = FALSE, stringsAsFactors = FALSE)$V1
  
  inds = grepl("^[A-Z]{2}.{5}", a)
  
  code = trimws(substr(a, 1, 7))[inds]
  name = trimws(substr(a, 10, nchar(a)))[inds]
  a = data.frame(parameters = code, label = name)
  return(a)
}
