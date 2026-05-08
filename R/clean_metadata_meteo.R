#' Meteorological metadata cleaning
#'
#' Internal function for meteorological metadata cleaning
#' @param address URL address of the metadata file
#' @param rank stations' rank
#' @param interval temporal interval
#' @importFrom utils read.fwf
#' @importFrom stats na.omit
#' @importFrom stringi stri_trans_general
#' @keywords internal 
#' @noRd

clean_metadata_meteo = function(address, rank = "synop", interval = "hourly") {

  temp = tempfile()
  test_url(link = address, output = temp)
  a = read.csv(temp, header = FALSE, stringsAsFactors = FALSE, 
               fileEncoding = "CP1250")$V1

  inds = grepl("^[A-Z]{2}.{5}", a)
  
  code = trimws(substr(a, 1, 7))[inds]
  name = trimws(substr(a, 8, nchar(a)))[inds]
  a = data.frame(parameters = code, label = name)
  a$label = stringi::stri_trans_general(a$label, 'LATIN-ASCII')
  return(a)
}
