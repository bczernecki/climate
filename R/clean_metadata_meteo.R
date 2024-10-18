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
  a = gsub(a, pattern = "\\?", replacement = "")
  a = stringi::stri_trans_general(a, 'LATIN-ASCII')

  # additional workarounds for mac os but not only...
  a = gsub(x = a, pattern = "'", replacement = "")
  a = gsub(x = a, pattern = "\\^0", replacement = "")
  a = data.frame(V1 = a[nchar(a) > 3], stringsAsFactors = FALSE)
  length_char = max(nchar(a$V1), na.rm = TRUE)

  if (rank == "precip" && interval == "hourly") length_char = 40 # exception for precip / hourly
  if (rank == "precip" && interval == "daily") length_char = 38 # exception for precip / daily
  if (rank == "synop" && interval == "hourly") length_char = 60 # exception for synop / hourly
  if (rank == "climate" && interval == "monthly") length_char = 52 # exception for climate / monthly

  field = substr(a$V1, length_char - 3, length_char)

  if (rank == "synop" && interval == "monthly") {
    length_char = as.numeric(names(sort(table(nchar(a$V1)), decreasing = TRUE)[1])) + 2
    field = substr(a$V1, length_char - 3, length_char + 2)
  }

  a$field1 = suppressWarnings(as.numeric(unlist(lapply(strsplit(field, "/"), function(x) x[1]))))
  a$field2 = suppressWarnings(as.numeric(unlist(lapply(strsplit(field, "/"), function(x) x[2]))))

  a$V1 = trimws(substr(a$V1, 1, nchar(a$V1) - 3))
  a$V1 = gsub(x = a$V1, pattern = "*  ", "")

  a = a[!(is.na(a$field1) & is.na(a$field2)), ] # remove info about status
  colnames(a)[1] = "parameters"
  return(a)
}
