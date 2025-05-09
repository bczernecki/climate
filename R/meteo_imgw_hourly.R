#' Hourly IMGW meteorological data
#'
#' Downloading hourly (meteorological) data from the SYNOP / CLIMATE / PRECIP stations
#' available in the danepubliczne.imgw.pl collection
#'
#' @param rank rank of the stations: "synop" (default), "climate", or "precip"
#' @param year vector of years (e.g., 1966:2000)
#' @param status leave the columns with measurement and observation statuses
#' (default status = FALSE - i.e. the status columns are deleted)
#' @param coords add coordinates of the station (logical value TRUE or FALSE)
#' @param station name or ID of meteorological station(s).
#' It accepts names (characters in CAPITAL LETTERS) or stations' IDs (numeric)
#' @param col_names three types of column names possible: "short" - default,
#' values with shorten names, "full" - full English description,
#' "polish" - original names in the dataset
#' @param allow_failure logical - whether to proceed or stop on failure. By default set to TRUE (i.e. don't stop on error). For debugging purposes change to FALSE
#' @param ... other parameters that may be passed to the 'shortening'
#' function that shortens column names
#' @importFrom XML readHTMLTable
#' @importFrom utils download.file unzip read.csv
#' @importFrom data.table fread
#' @importFrom archive archive_read
#' @export
#' @return meteorological data for the hourly time interval
#'
#' @examples \donttest{
#'   hourly = meteo_imgw_hourly(rank = "climate", year = 1984)
#'   head(hourly)
#' }
#'


meteo_imgw_hourly = function(rank = "synop",
                             year,
                             status = FALSE,
                             coords = FALSE,
                             station = NULL,
                             col_names = "short", 
                             allow_failure = TRUE,
                             ...) {
  
  if (allow_failure) {
    tryCatch(meteo_imgw_hourly_bp(rank,
                                  year,
                                  status,
                                  coords,
                                  station,
                                  col_names, ...),
             error = function(e){
               message(paste("Potential error(s) found. Problems with downloading data.\n",
                             "\rRun function with argument allow_failure = FALSE",
                             "to see more details"))})
  } else {
    meteo_imgw_hourly_bp(rank,
                         year,
                         status,
                         coords,
                         station,
                         col_names, ...)
  }
}

#' @keywords internal
#' @noRd
meteo_imgw_hourly_bp = function(rank,
                                year,
                                status,
                                coords,
                                station,
                                col_names, ...) {
  
  translit = check_locale()
  stopifnot(rank == "synop" | rank == "climate") # dla terminowek tylko synopy i klimaty maja dane
  base_url = "https://danepubliczne.imgw.pl/data/dane_pomiarowo_obserwacyjne/"
  interval = "hourly"
  interval_pl = "terminowe"
  meta = meteo_metadata_imgw(interval = "hourly", rank = rank)
  rank_pl = switch(rank, synop = "synop", climate = "klimat", precip = "opad")
  temp = tempfile()
  test_url(link = paste0(base_url, "dane_meteorologiczne/", interval_pl, "/", rank_pl, "/"),
           output = temp)
  a = readLines(temp, warn = FALSE)
  unlink(temp)
  
  ind = grep(readHTMLTable(a)[[1]]$Name, pattern = "/")
  catalogs = as.character(readHTMLTable(a)[[1]]$Name[ind])

  years_in_catalogs = strsplit(gsub(x = catalogs, pattern = "/", replacement = ""), split = "_")
  years_in_catalogs = lapply(years_in_catalogs, function(x) x[1]:x[length(x)])
  ind = lapply(years_in_catalogs, function(x) sum(x %in% year) > 0)
  catalogs = catalogs[unlist(ind)] # to sa nasze prawdziwe catalogs do przemielenia

  all_data = NULL

  for (i in seq_along(catalogs)) {
    catalog = gsub(catalogs[i], pattern = "/", replacement = "")

    if (rank == "synop") {
      address = paste0(base_url, "dane_meteorologiczne/terminowe/synop",
                        "/", catalog, "/")

      test_url(link = address, output = temp)
      folder_contents = readLines(temp, warn = FALSE)
      unlink(temp)

      ind = grep(readHTMLTable(folder_contents)[[1]]$Name, pattern = "zip")
      files = as.character(readHTMLTable(folder_contents)[[1]]$Name[ind])
      
      addresses_to_download = paste0(address, files)

      for (j in seq_along(addresses_to_download)) {
        temp = tempfile()
        temp2 = tempfile()
        test_url(addresses_to_download[j], temp)
        unzip(zipfile = temp, exdir = temp2)
        file1 = paste(temp2, dir(temp2), sep = "/")
        data1 = imgw_read(translit, file1)
        colnames(data1) = meta[[1]]$parameters

        # remove statuses
        if (status == FALSE) {
          data1[grep("^Status", colnames(data1))] = NULL
        }

        unlink(c(temp, temp2))
        all_data[[length(all_data) + 1]] = data1
      } # koniec petli po zipach do pobrania
    } # koniec if'a dla synopa

    ######################
    ###### KLIMAT: #######
    ######################
    if (rank == "climate") {
      address = paste0(base_url, "dane_meteorologiczne/terminowe/klimat",
                        "/", catalog, "/")

      test_url(link = address, output = temp)
      folder_contents = readLines(temp, warn = FALSE)
      unlink(temp)

      ind = grep(readHTMLTable(folder_contents)[[1]]$Name, pattern = "zip")
      files = as.character(readHTMLTable(folder_contents)[[1]]$Name[ind])
      addresses_to_download = paste0(address, files)

      for (j in seq_along(addresses_to_download)) {
        temp = tempfile(fileext = ".zip")
        temp2 = tempfile()
        test_url(addresses_to_download[j], temp)
        d = tryCatch(expr = unzip(zipfile = temp, exdir = temp2), 
                     warning = function(w) {
                       env$logs = c(env$logs, 
                                    paste("Warning: ", w$message, " ",
                                          addresses_to_download[j], sep = ""))
                       # try to read it with archive package:
                       data = archive_read(temp, file = paste0("k_t_", sprintf("%02d", j), "_", year, ".csv"), format = "zip")
                       csv_data = read.csv(data, header = FALSE, sep = ",")
                       csv_data = convert_encoding(csv_data)
                       colnames(csv_data) = meta[[1]]$parameters
                       csv_data$`Nazwa stacji` = trimws(csv_data$`Nazwa stacji`)
                       return(csv_data)
                       })

        if (!is.null(d) & !is.data.frame(d)) {
          file1 = paste(temp2, dir(temp2), sep = "/")
          data1 = imgw_read(translit, file1)
        } else if (is.data.frame(d)) {
          data1 = d
        }
        
        colnames(data1) = meta[[1]]$parameters
        # remove status
        if (status == FALSE) {
          data1[grep("^Status", colnames(data1))] = NULL
          }
        unlink(c(temp, temp2))
        all_data[[length(all_data) + 1]] = data1
      } # end of looping for zip files
    } # end of if statement for climate
  } # end of loop over directories

  if (!is.null(all_data)) {
    all_data = do.call(rbind, all_data)
  } else {
    stop("No data found. Quitting", call. = FALSE)
  }

  if (coords) {
    all_data = merge(climate::imgw_meteo_stations[, 1:3], 
                     all_data, 
                     by.x = "id", 
                     by.y = "Kod stacji", 
                     all.y = TRUE)
  }

  # add rank
  rank_code = switch(rank, synop = "SYNOPTYCZNA", climate = "KLIMATYCZNA")
  all_data = cbind(data.frame(rank_code = rank_code), all_data)
  all_data = all_data[all_data$Rok %in% year, ] # przyciecie tylko do wybranych lat gdyby sie pobralo za duzo

  # station selection
  if (!is.null(station)) {
    if (is.character(station)) {
      inds = as.numeric(sapply(station, function(x) grep(pattern = x, x = all_data$`Nazwa stacji`)))
      all_data = all_data[inds, ]
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
  all_data$`Nazwa stacji` = trimws(all_data$`Nazwa stacji`)

  # sortowanie w zaleznosci od nazw kolumn - raz jest "kod stacji", raz "id"
  if (sum(grepl(x = colnames(all_data), pattern = "Kod stacji"))) {
    all_data = all_data[order(all_data$`Kod stacji`,
                              all_data$Rok,
                              all_data$Miesiac,
                              all_data$Dzien,
                              all_data$Godzina), ]
  } else {
    all_data = all_data[order(all_data$id, all_data$Rok, all_data$Miesiac, all_data$Dzien, all_data$Godzina), ]
  }

  # extra option for shortening colnames and removing duplicates
  all_data = meteo_shortening_imgw(all_data, col_names = col_names, ...)
  rownames(all_data) = NULL
  
  # check if there any messages gathered in env$logs and if it is not empty then print them:
  if (length(env$logs) > 0) {
    message("\nPotential error(s) found.\nPlease carefully check content of files derived from:\n",
            paste(env$logs, collapse = "\n"))
    env$logs = NULL
  }
  return(all_data)
}
