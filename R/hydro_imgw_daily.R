#' Daily hydrological data
#'
#' Downloading daily hydrological data from the danepubliczne.imgw.pl collection
#'
#' @param year vector of years (e.g., 1966:2000)
#' @param coords add coordinates of the stations (logical value TRUE or FALSE)
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
#' @returns data.frame with historical hydrological data for the daily time interval
#' @examples \donttest{
#'   daily = hydro_imgw_daily(year = 2000)
#' }
#'

hydro_imgw_daily = function(year,
                            coords = FALSE,
                            station = NULL,
                            col_names= "short",
                            allow_failure = TRUE,
                            ...) {
  
  if (allow_failure) {
    tryCatch(hydro_imgw_daily_bp(year,
                                 coords,
                                 station,
                                 col_names, 
                                 ...),
             error = function(e){
               message(paste("Problems with downloading data.",
                             "Run function with argument allow_failure = FALSE",
                             "to see more details"))})
  } else {
    hydro_imgw_daily_bp(year,
                        coords,
                        station,
                        col_names, 
                        ...)
  }
}

#' @keywords internal
#' @noRd
hydro_imgw_daily_bp = function(year,
                               coords,
                               station,
                               col_names,
                               ...) {
  translit = check_locale()
  base_url = "https://danepubliczne.imgw.pl/data/dane_pomiarowo_obserwacyjne/dane_hydrologiczne/"
  interval = "daily"
  interval_pl = "dobowe"

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
    iterator = c("01", "02", "03", "04", "05", "06",
                "07", "08", "09", "10", "11", "12")
    data = NULL
    for (j in seq_along(iterator)) {
      address = paste0(base_url, interval_pl, "/", catalog, "/codz_", catalog, "_", iterator[j], ".zip")
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
      # extra exception for a current year according to information provided by IMGW-PIB:
      # i.e.:
      # "Do czasu zakonczenia kontroli przeplywow rekordy z danymi z roku 2020 maja format:
      #Kod stacji  #Nazwa stacji  #Nazwa rzeki/jeziora  #Rok hydrologiczny  #Wskaznik miesiaca w roku hydrologicznym
      #Dzien  #Stan wody [cm]  #Temperatura wody [st. C]    #Miesiac kalendarzowy
      if (ncol(data1) == 9) {
        data1$flow = NA
        data1 = data1[, c(1:7, 10, 8:9)]
      }

      colnames(data1) = meta[[1]][, 1]
      data = rbind(data, data1)
    }
    address = paste0(base_url, interval_pl, "/", catalog, "/zjaw_", catalog, ".zip")

    temp = tempfile()
    temp2 = tempfile()
    test_url(address, temp)
    unzip(zipfile = temp, exdir = temp2)
    file2 = paste(temp2, dir(temp2), sep = "/")[1]

    if (translit) {
      data2 = as.data.frame(data.table::fread(cmd = paste("iconv -f CP1250 -t ASCII//TRANSLIT", file2)))
    } else {
      data2 = read.csv(file2, header = FALSE, stringsAsFactors = FALSE, fileEncoding = "CP1250")
    }

    colnames(data2) = meta[[2]][, 1]
    all_data[[i]] = merge(data, data2,
                         by = intersect(colnames(data), colnames(data2)),
                         all.x = TRUE)
  }

  all_data = do.call(rbind, all_data)
  all_data[all_data == 9999] = NA
  all_data[all_data == 99999.999] = NA
  all_data[all_data == 99.9] = NA
  all_data[all_data == 999] = NA

  if (coords) {
    all_data = merge(climate::imgw_hydro_stations, all_data,
                     by.x = "id",
                     by.y = "Kod stacji",
                     all.y = TRUE)
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

  all_data = all_data[do.call(order, all_data[grep(x = colnames(all_data), "Nazwa stacji|Rok hydro|w roku hydro|Dzie")]), ]
  all_data = hydro_shortening_imgw(all_data, col_names = col_names, ...)

  return(all_data)
}
