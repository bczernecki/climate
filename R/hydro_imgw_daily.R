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
                            col_names = "short",
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
  
  # initiate empty objects:
  all_data = NULL
  codz_data = NULL
  zjaw_data = NULL
  
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
  
  for (i in seq_along(catalogs)) {
    catalog = catalogs[i]
    
    temp = tempfile()
    test_url(link = paste0(base_url, interval_pl, "/", catalog), output = temp)
    b = readLines(temp, warn = FALSE)
    
    files_in_dir = readHTMLTable(b)[[1]]$Name
    ind = grep(files_in_dir, pattern = "zip")
    codz_files = grep(x = files_in_dir, pattern = "codz", value = TRUE)
    zjaw_files = grep(x = files_in_dir, pattern = "zjaw", value = TRUE)
    iterator = c(codz_files, zjaw_files)
    
    for (j in seq_along(iterator)) {
      
      # file pattern for codz:
      if (grepl(x = iterator[j], "codz")) {
        address = paste0(base_url, interval_pl, "/", catalog, "/", iterator[j])
        temp = tempfile()
        temp2 = tempfile()
        test_url(link = address, output = temp)
        unzip(zipfile = temp, exdir = temp2)
        file1 = paste(temp2, dir(temp2), sep = "/")[1]
        
        data1 = imgw_read(translit, file1)
        # extra exception for a current year according to information provided by IMGW-PIB:, i.e.:
        # "Do czasu zakonczenia kontroli przeplywow rekordy z danymi z roku 2020 maja format:
        # Kod stacji  #Nazwa stacji  #Nazwa rzeki/jeziora  #Rok hydrologiczny  #Wskaznik miesiaca w roku hydrologicznym
        # Dzien  #Stan wody [cm]  #Temperatura wody [st. C]    #Miesiac kalendarzowy
        if (ncol(data1) == 9) {
          data1$flow = NA
          data1 = data1[, c(1:7, 10, 8:9)]
        }
        
        colnames(data1) = meta[[1]][, 1]
        codz_data = rbind(codz_data, data1)
      } # end of codz_
      
      
      # start of zjaw_ section:
      if (grepl(x = iterator[j], "zjaw")) {
        address = paste0(base_url, interval_pl, "/", catalog, "/", iterator[j])
        temp = tempfile()
        temp2 = tempfile()
        test_url(address, temp)
        unzip(zipfile = temp, exdir = temp2)
        file2 = paste(temp2, dir(temp2), sep = "/")[1]
        data2 = imgw_read(translit, file2)
        colnames(data2) = meta[[2]][, 1]
        zjaw_data = rbind(zjaw_data, data2)
      }
      
    } #end of loop for (usually monthly) zip files in a given year
    
    all_data[[length(all_data) + 1]] = merge(codz_data, zjaw_data,
                                             by = intersect(colnames(codz_data), colnames(zjaw_data)),
                                             all.x = TRUE)
    
  } # end of loop for years (if more than 1 specified)
  
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
  # fix dates and add as seperate column:
  yy_ind = grep(x = colnames(all_data), "Rok hydrologiczny")
  mm_ind = grep(x = colnames(all_data), "kalendarzowy")
  dd_ind = grep(x = colnames(all_data), "Dzie")
  data_df = all_data[, c(yy_ind, mm_ind, dd_ind)]
  data_df$yy = ifelse(data_df[, 2] >= 11, data_df[, 1] - 1, data_df[, 1])
  all_data$Data = as.Date(ISOdate(year = data_df$yy, month = data_df[, 2], day = data_df[, 3]))
  all_data = all_data[, c(1:3, ncol(all_data), 4:(ncol(all_data) - 1)), ]
  
  all_data = hydro_shortening_imgw(all_data, col_names = col_names, ...)
  
  return(all_data)
}