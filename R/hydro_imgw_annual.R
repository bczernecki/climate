#' Semi-annual and annual hydrological data
#'
#' Downloading hydrological data for the semi-annual and annual period
#' available in the danepubliczne.imgw.pl collection
#'
#' @param year vector of years (e.g., 1966:2000)
#' @param coords add coordinates of the stations (logical value TRUE or FALSE)
#' @param value type of data (can be: state - "H" (default), flow - "Q", or temperature - "T")
#' @param station name or ID of hydrological station(s).
#' It accepts names (characters in CAPITAL LETTERS) or stations' IDs (numeric)
#' @param col_names three types of column names possible:
#' "short" - default, values with shorten names,
#' "full" - full English description,
#' "polish" - original names in the dataset
#' @param allow_failure logical - whether to proceed or stop on failure. By default set to TRUE (i.e. don't stop on error). For debugging purposes change to FALSE
#' @param ... other parameters that may be passed to the 'shortening' function that shortens column names
#' @importFrom XML readHTMLTable
#' @importFrom utils download.file unzip read.csv
#' @importFrom data.table fread
#' @export
#' @returns data.frame with historical hydrological data for the semi-annual and annual period
#' @examples
#' \donttest{
#' hydro_yearly = hydro_imgw_annual(year = 2000, value = "H", station = "ANNOPOL")
#' }
hydro_imgw_annual = function(year,
                             coords = FALSE,
                             value = "H",
                             station = NULL,
                             col_names = "short", 
                             allow_failure = TRUE,
                             ...) {
  
  if (allow_failure) {
    tryCatch(hydro_imgw_annual_bp(year,
                                  coords,
                                  value,
                                  station,
                                  col_names, 
                                  ...),
             error = function(e){
               message(paste("Problems with downloading data.",
                             "Run function with argument allow_failure = FALSE",
                             "to see more details"))})
  } else {
    hydro_imgw_annual_bp(year,
                         coords,
                         value,
                         station,
                         col_names, 
                         ...)
  }
}

#' @keywords internal
#' @noRd
hydro_imgw_annual_bp = function(year = year,
                                coords = coords,
                                value = value,
                                station = station,
                                col_names = col_names,
                                ...) {
  
  translit = check_locale()
  base_url = "https://danepubliczne.imgw.pl/data/dane_pomiarowo_obserwacyjne/dane_hydrologiczne/"
  interval = "semiannual_and_annual"
  interval_pl = "polroczne_i_roczne"

  temp = tempfile()
  test_url(link = paste0(base_url, interval_pl, "/"), output = temp)
  a = readLines(temp, warn = FALSE)

  ind = grep(readHTMLTable(a)[[1]]$Name, pattern = "/")
  catalogs = as.character(readHTMLTable(a)[[1]]$Name[ind])
  catalogs = gsub(x = catalogs, pattern = "/", replacement = "")

  catalogs = catalogs[catalogs %in% as.character(year)]
  if (length(catalogs) == 0) {
    stop("Selected year(s) is/are not available in the database.", call. = FALSE)
  }
  meta = hydro_metadata_imgw(interval)

  all_data = vector("list", length = length(catalogs))
  for (i in seq_along(catalogs)) {
    catalog = catalogs[i]
    address = paste0(base_url, interval_pl, "/", catalog, "/polr_", value, "_", catalog, ".zip")

    temp = tempfile()
    temp2 = tempfile()
    test_url(address, temp)
    #download.file(address, temp)
    unzip(zipfile = temp, exdir = temp2)
    file1 = paste(temp2, dir(temp2), sep = "/")[1]

    if (translit) {
      data1 = as.data.frame(data.table::fread(cmd = paste("iconv -f CP1250 -t ASCII//TRANSLIT", file1)))
    } else {
      data1 = read.csv(file1, header = FALSE, stringsAsFactors = FALSE, fileEncoding = "CP1250")
    }

    colnames(data1) = meta[[value]]$parameters
    all_data[[i]] = data1
  }
  all_data = do.call(rbind, all_data)
  all_data[all_data == 99999.999] = NA
  all_data = all_data[, !duplicated(colnames(all_data))]

  # coords
  if (coords) {
    all_data = merge(climate::imgw_hydro_stations, all_data, by.x = "id", by.y = "Nazwa rzeki/jeziora", all.y = TRUE)
  }
  #station selection
  if (!is.null(station)) {
    if (is.character(station)) {
      all_data = all_data[substr(all_data$`Nazwa stacji`, 1, nchar(station)) == station, ]
      if (nrow(all_data) == 0) {
        
        stop("Selected station(s) is not available in the database.", call. = FALSE)
      }
    } else if (is.numeric(station)) {
      all_data = all_data[all_data$`Kod stacji` %in% station, ]
      if (nrow(all_data) == 0) {
        stop("Selected station(s) is not available in the database.", call. = FALSE)
      }
    } else {
      stop("Selected station(s) are not in the proper format.", call. = FALSE)
    }
  }

  all_data = all_data[order(all_data$`Nazwa stacji`, all_data$`Rok hydrologiczny`), ]
  # adding option for shortening column names and removing duplicates
  all_data = hydro_shortening_imgw(all_data, col_names = col_names, ...)

  return(all_data)
}
