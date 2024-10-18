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
  if (interval == "semiannual_and_annual") {
    godzina = paste0(a[13], ":", a[14])
    data = c(a[10:12], godzina)
    data_od = paste0("wystapienie_od_", data)
    data_do = paste0("wystapienie_do_", data)
    SPT = unlist(strsplit(a[8], "]/")) # stan/przeplyw/temperatura
    SPT[1] = paste0(SPT[1], "]")
    SPT[2] = paste0(SPT[2], "]")
    b = NULL
    for (i in seq_along(SPT)) {
      tmp = c(a[1:7], SPT[i], data_od, data_do)
      b = cbind(b, tmp)
    }
    b = list("H" = data.frame(parameters = b[, 1]),
             "Q" = data.frame(parameters = b[, 2]),
             "T" = data.frame(parameters = b[, 3]))
  }
  return(b)
}
