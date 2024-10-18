#' Daily IMGW meteorological data
#'
#' Downloading daily (meteorological) data from the SYNOP / CLIMATE / PRECIP stations
#' available in the danepubliczne.imgw.pl collection
#'
#' @param rank rank of the stations: "synop" (default), "climate", or "precip"
#' @param year vector of years (e.g., 1966:2000)
#' @param status leave the columns with measurement and observation statuses
#' (default status = FALSE - i.e. the status columns are deleted)
#' @param coords add coordinates of the station (logical value TRUE or FALSE)
#' @param station name of meteorological station(s).
#' It accepts names (characters in CAPITAL LETTERS); Stations' IDs (numeric) are no longer valid
#' @param col_names three types of column names possible:
#' "short" - default, values with shorten names,
#' "full" - full English description,
#' "polish" - original names in the dataset
#' @param allow_failure logical - whether to proceed or stop on failure. By default set to TRUE (i.e. don't stop on error). For debugging purposes change to FALSE
#' @param ... other parameters that may be passed to the 'shortening' function that
#' shortens column names
#' @importFrom XML readHTMLTable
#' @importFrom utils download.file unzip read.csv
#' @importFrom data.table fread
#' @returns data.frame with a daily meteorological measurements
#' @export
#'
#' @examples \donttest{
#'   daily = meteo_imgw_daily(rank = "climate", year = 2000)
#' }
#'

meteo_imgw_daily = function(rank = "synop",
                            year,
                            status = FALSE,
                            coords = FALSE,
                            station = NULL,
                            col_names = "short", 
                            allow_failure = TRUE,
                            ...) {
  
  if (allow_failure) {
    tryCatch(meteo_imgw_daily_bp(rank,
                                 year,
                                 status,
                                 coords,
                                 station,
                                 col_names),
             error = function(e){
               message(paste("Potential error(s) found. Problems with downloading data.\n",
                             "\rRun function with argument allow_failure = FALSE",
                             "to see more details"))})
  } else {
    meteo_imgw_daily_bp(rank,
                        year,
                        status,
                        coords,
                        station,
                        col_names,
                        ...)
  }
}

#' @keywords internal
#' @noRd
meteo_imgw_daily_bp = function(rank,
                               year,
                               status,
                               coords,
                               station,
                               col_names,
                               ...) {

  translit = check_locale()
  base_url = "https://danepubliczne.imgw.pl/data/dane_pomiarowo_obserwacyjne/"
  interval = "daily"
  interval_pl = "dobowe"
  meta = meteo_metadata_imgw(interval = "daily", rank = rank)
  rank_pl = switch(rank, synop = "synop", climate = "klimat", precip = "opad")

  temp = tempfile()
  test_url(link = paste0(base_url, "dane_meteorologiczne/", interval_pl, "/", rank_pl, "/"),
          output = temp)
  a = readLines(temp, warn = FALSE)
  unlink(temp)

  ind = grep(readHTMLTable(a)[[1]]$Name, pattern = "/")
  catalogs = as.character(readHTMLTable(a)[[1]]$Name[ind])

  years_in_catalogs = strsplit(gsub(x = catalogs, pattern = "/", replacement = ""),
                               split = "_")
  years_in_catalogs = lapply(years_in_catalogs, function(x) x[1]:x[length(x)])
  ind = lapply(years_in_catalogs, function(x) sum(x %in% year) > 0)
  catalogs = catalogs[unlist(ind)]

  all_data = NULL

  for (i in seq_along(catalogs)) {
    catalog = gsub(catalogs[i], pattern = "/", replacement = "")

    if (rank == "synop") {
      address = paste0(base_url, "/dane_meteorologiczne/dobowe/synop", #nolint
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
        file1 = paste(temp2, dir(temp2), sep = "/")[1]
        if (translit) {
          data1 = as.data.frame(data.table::fread(cmd = paste("iconv -f CP1250 -t ASCII//TRANSLIT", file1)))
        } else {
          data1 = read.csv(file1, header = FALSE, stringsAsFactors = FALSE, fileEncoding = "CP1250")
        }
        colnames(data1) = meta[[1]]$parameters

        file2 = paste(temp2, dir(temp2), sep = "/")[2]
        if (translit) {
          data2 = data.table::fread(cmd = paste("iconv -f CP1250 -t ASCII//TRANSLIT", file2))
        } else {
          data2 = suppressWarnings(read.csv(file2, header = FALSE, stringsAsFactors = FALSE, fileEncoding = "CP1250"))
        }
        colnames(data2) = meta[[2]]$parameters
        unlink(c(temp, temp2))

        # remove statuses if not needed:
        if (status == FALSE) {
          data1[grep("^Status", colnames(data1))] = NULL
          data2[grep("^Status", colnames(data2))] = NULL
        }

        ttt = base::merge(data1,
                          data2,
                          by = c("Kod stacji", "Rok", "Miesiac", "Dzien"),
                          all.x = TRUE)
        
        ttt = ttt[order(ttt$`Nazwa stacji.x`, ttt$Rok, ttt$Miesiac, ttt$Dzien), ]
        ### ta część kodu powtarza sie po dużej petli od rank
        if (!is.null(station)) {
          all_data[[length(all_data) + 1]] = ttt[substr(ttt$`Nazwa stacji.x`, 1,
                                                        nchar(station)) %in% station, ]
        } else {
          all_data[[length(all_data) + 1]] = ttt
        }
      } # end of looping for zip archives
    } # end of if statement for SYNOP stations

    ######################
    ###### KLIMAT: #######
    if (rank == "climate") {
      address = paste0(base_url, "dane_meteorologiczne/dobowe/klimat",
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
        file1 = paste(temp2, dir(temp2), sep = "/")[1]
        if (translit) {
          data1 = as.data.frame(data.table::fread(cmd = paste("iconv -f CP1250 -t ASCII//TRANSLIT", file1)))
        } else {
          data1 = read.csv(file1, header = FALSE, stringsAsFactors = FALSE, fileEncoding = "CP1250")
        }
        colnames(data1) = meta[[1]]$parameters

        file2 = paste(temp2, dir(temp2), sep = "/")[2]
        if (translit) {
          data2 = as.data.frame(data.table::fread(cmd = paste("iconv -f CP1250 -t ASCII//TRANSLIT", file2)))
        } else {
          data2 = read.csv(file2, header = FALSE, stringsAsFactors = FALSE, fileEncoding = "CP1250")
        }
        colnames(data2) = meta[[2]]$parameters

        # usuwa statusy
        if (status == FALSE) {
          data1[grep("^Status", colnames(data1))] = NULL
          data2[grep("^Status", colnames(data2))] = NULL
        }

        unlink(c(temp, temp2))
        all_data[[length(all_data) + 1]] = merge(data1,
                                                 data2,
                                                 by = c("Kod stacji", "Rok", "Miesiac", "Dzien"),
                                                 all.x = TRUE)
      } # end of looping for zip files
    } # end of if statement for climate stations

    ########################
    ######## PRECIP: #######
    if (rank == "precip") {
      address = paste0(base_url, "dane_meteorologiczne/dobowe/opad",
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
        file1 = paste(temp2, dir(temp2), sep = "/")[1]
        if (translit) {
          data1 = as.data.frame(data.table::fread(cmd = paste("iconv -f CP1250 -t ASCII//TRANSLIT", file1)))
        } else {
          data1 = read.csv(file1, header = FALSE, stringsAsFactors = FALSE, fileEncoding = "CP1250")
        }

        colnames(data1) = meta[[1]]$parameters
        # usuwa statusy
        if (status == FALSE) {
          data1[grep("^Status", colnames(data1))] = NULL
        }

        unlink(c(temp, temp2))
        all_data[[length(all_data) + 1]] = data1
      } # end of loop for zip files
    } # end of if statement for climate stations
  } # end of looping over catalogs

  all_data = do.call(rbind, all_data)

  if (coords) {
    all_data = merge(climate::imgw_meteo_stations[, 1:3],
                     all_data,
                     by.x = "id",
                     by.y = "Kod stacji",
                     all.y = TRUE)
  }

  # add station rank:
  rank_code = switch(rank, synop = "SYNOPTYCZNA", climate = "KLIMATYCZNA", precip = "OPADOWA")
  all_data = cbind(data.frame(rank_code = rank_code), all_data)

  all_data = all_data[all_data$Rok %in% year, ] # clip only to selected years

  #station selection
  if (!is.null(station)) {
    if (is.character(station)) {
      if (rank == "synop" | rank == "climate") {
        inds = as.numeric(sapply(station, function(x) grep(pattern = x, x = all_data$`Nazwa stacji.x`)))
        all_data = all_data[inds, ]
      }

      # exception for column names in precipitation data:
      if (rank == "precip") {
        inds = as.numeric(sapply(station, function(x) grep(pattern = x, x = all_data$`Nazwa stacji`)))
        all_data = all_data[inds, ]
      }

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

  # sort output
  if (sum(grepl(x = colnames(all_data), pattern = "Kod stacji"))) {
    all_data = all_data[order(all_data$`Kod stacji`, all_data$Rok, all_data$Miesiac, all_data$Dzien), ]
  } else {
    all_data = all_data[order(all_data$id, all_data$Rok, all_data$Miesiac, all_data$Dzien), ]
  }

  # remove duplicates and shorten colnames
  all_data = meteo_shortening_imgw(all_data, col_names = col_names, ...)
  rownames(all_data) = NULL

  return(all_data)
}
