#' Semi-annual and annual hydrological data
#'
#' Downloading hydrological data for the semi-annual and annual period
#' available in the dane.imgw.pl collection
#'
#' @param year vector of years (e.g., 1966:2000)
#' @param coords add coordinates of the stations (logical value TRUE or FALSE)
#' @param value type of data (can be: state - "H" (default), flow - "Q", or temperature - "T")
#' @param station name or ID of hydrological station(s).
#' It accepts names (characters in CAPITAL LETTERS) or stations' IDs (numeric)
#' @param col_names three types of column names possible: "short" - default, values with shorten names, "full" - full English description, "polish" - original names in the dataset
#' @param ... other parameters that may be passed to the 'shortening' function that shortens column names
#' @importFrom XML readHTMLTable
#' @importFrom utils download.file unzip read.csv
#' @export
#'
#' @examples
#' \donttest{
#'   yearly = hydro_imgw_annual(year = 2000, value = "H", station = "ANNOPOL")
#'   head(yearly)
#' }
hydro_imgw_annual =  function(year, coords = FALSE, value = "H", station = NULL, col_names = "short", ...){

 # options(RCurlOptions = list(ssl.verifypeer = FALSE)) # required on windows for RCurl

  base_url = "https://dane.imgw.pl/data/dane_pomiarowo_obserwacyjne/dane_hydrologiczne/"
  interval = "semiannual_and_annual"
  interval_pl = "polroczne_i_roczne"
  
  temp = tempfile()
  test_url(link = paste0(base_url, interval_pl, "/"), output = temp)
  a = readLines(temp, warn = FALSE)
  
  # 
  # tryCatch(
  #   a = getURL(paste0(base_url, interval_pl, "/"),
  #               ftp.use.epsv = FALSE,
  #               dirlistonly = TRUE)
  # } else {
  #   stop(call. = FALSE, 
  #        paste0("\nDownload failed. ",
  #               "Check your internet connection or validate this url in your browser: ",
  #               paste0(base_url, interval_pl, "/"), "\n"))
  # }
  # 
  
  ind = grep(readHTMLTable(a)[[1]]$Name, pattern = "/")
  catalogs = as.character(readHTMLTable(a)[[1]]$Name[ind])
  catalogs = gsub(x = catalogs, pattern = "/", replacement = "")
  # less files to read:
  catalogs = catalogs[catalogs %in% as.character(year)]
  if (length(catalogs) == 0) {
    stop("Selected year(s) is/are not available in the database.", call. = FALSE)
  }
  meta = hydro_metadata_imgw(interval)

  all_data = vector("list", length = length(catalogs))
  for (i in seq_along(catalogs)){
    # i = 1
    catalog = catalogs[i]
    #print(i)

    address = paste0(base_url, interval_pl, "/", catalog, "/polr_", value, "_", catalog, ".zip")

    temp = tempfile()
    temp2 = tempfile()
    test_url(address, temp)
    #download.file(address, temp)
    unzip(zipfile = temp, exdir = temp2)
    file1 = paste(temp2, dir(temp2), sep = "/")[1]
    data1 = read.csv(file1, header = FALSE, stringsAsFactors = FALSE, fileEncoding = "CP1250")
    colnames(data1) = meta[[value]]$parameters
    all_data[[i]] = data1
  }
  all_data = do.call(rbind, all_data)
  # ten sam warunek braku danych lub obserwacji dla wszytkich wartosci
  all_data[all_data == 99999.999] = NA
  all_data = all_data[, !duplicated(colnames(all_data))]

  # coords
  if (coords){
    all_data = merge(climate::imgw_hydro_stations, all_data, by.x = "id", by.y = "Kod stacji", all.y = TRUE)
  }
  #station selection
  if (!is.null(station)) {
    if (is.character(station)) {
      all_data = all_data[substr(all_data$`Nazwa stacji`,1,nchar(station))==station, ]
      if (nrow(all_data) == 0){
        stop("Selected station(s) is not available in the database.", call. = FALSE)
      }
    } else if (is.numeric(station)){
      all_data = all_data[all_data$`Kod stacji` %in% station, ]
      if (nrow(all_data) == 0){
        stop("Selected station(s) is not available in the database.", call. = FALSE)
      }
    } else {
      stop("Selected station(s) are not in the proper format.", call. = FALSE)
    }
  }

  all_data = all_data[order(all_data$`Nazwa stacji`, all_data$`Rok hydrologiczny`), ]
  # dodanie opcji  dla skracania kolumn i usuwania duplikatow:
  all_data = hydro_shortening_imgw(all_data, col_names = col_names, ...)

  return(all_data)
}
