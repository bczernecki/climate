#' Read IMGW hydrological and meteorological raw files that can be saved in different formats
#'
#' Internal function for reading IMGW files
#' @param translit logical whether translit detected and iconv needed for reading
#' @param fpath path to unzipped CSV-alike file
#' 
#' @keywords internal
#' @noRd

imgw_read = function(translit, fpath) {

  if (translit) {
    data = as.data.frame(data.table::fread(cmd = paste("iconv -f ISO-8859-2 -t ASCII//TRANSLIT", fpath)))
  } else {
    data = tryCatch(expr = read.csv(fpath, header = FALSE, stringsAsFactors = FALSE, sep = ",",
                                    fileEncoding = "CP1250"),
           warning = function(w) {
             read.csv(fpath, header = FALSE, stringsAsFactors = FALSE, sep = ";")
           })
    
    if (ncol(data) == 1) {
      data = tryCatch(expr = read.csv(fpath, header = FALSE, stringsAsFactors = FALSE, sep = ";",
                                      fileEncoding = "UTF-8"),
                      warning = function(w) {
                        read.csv(fpath, header = FALSE, stringsAsFactors = FALSE, sep = ";")
                      })
    }
    
    # if still one column try the default option:
    if (ncol(data) == 1) {
      data = suppressWarnings(read.csv(fpath, 
                                       header = FALSE, 
                                       stringsAsFactors = FALSE, 
                                       sep = ",",
                                       quote = "\"",
                                       fileEncoding = "CP1250"))
    }
    
    # if still one column but data inside, then try to split it by commas with read.csv:
    if (ncol(data) == 1) {
      data = read.csv(text = data$V1, header = FALSE, stringsAsFactors = FALSE)
      }
    
    }
  return(data)
}
