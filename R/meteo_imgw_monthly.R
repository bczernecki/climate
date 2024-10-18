#' Monthly IMGW meteorological data
#'
#' Downloading monthly (meteorological) data from the
#' SYNOP / CLIMATE / PRECIP stations available in the danepubliczne.imgw.pl collection
#'
#' @param rank rank of the stations: "synop" (default), "climate", or "precip"
#' @param year vector of years (e.g., 1966:2000)
#' @param status leave the columns with measurement and observation statuses
#' (default status = FALSE - i.e. the status columns are deleted)
#' @param coords add coordinates of the station (logical value TRUE or FALSE)
#' @param station name or ID of meteorological station(s).
#' It accepts names (characters in CAPITAL LETTERS) or stations' IDs (numeric).
#' Please note that station names may change over time and thus sometimes 2 names 
#' are required in some cases, e.g. `c("POZNAŃ", "POZNAŃ-ŁAWICA")`.
#' @param col_names three types of column names possible: "short" - default,
#' values with shorten names, "full" - full English description,
#' "polish" - original names in the dataset
#' @param allow_failure logical - whether to proceed or stop on failure. 
#' By default set to TRUE (i.e. don't stop on error). For debugging purposes change to FALSE
#' @param ... other parameters that may be passed to the
#' 'shortening' function that shortens column names
#' @importFrom XML readHTMLTable
#' @importFrom utils unzip read.csv
#' @importFrom data.table fread
#' @export
#' @return meteorological data with monthly summaries
#'
#' @examples 
#' \donttest{
#'   monthly = meteo_imgw_monthly(rank = "climate", year = 1969)
#'   head(monthly)
#'
#'   # a descriptive (long) column names:
#'   monthly2 = meteo_imgw_monthly(rank = "synop", year = 2018,
#'          col_names = "full")
#'   head(monthly2)
#' }
#'
meteo_imgw_monthly = function(rank = "synop",
                              year,
                              status = FALSE,
                              coords = FALSE,
                              station = NULL,
                              col_names = "short", 
                              allow_failure = TRUE,
                              ...) {

  if (allow_failure) {
    tryCatch(meteo_imgw_monthly_bp(rank,
                                   year,
                                   status,
                                   coords,
                                   station,
                                   col_names,
                                   ...),
             error = function(e){
               message(paste("Potential error(s) found. Problems with downloading data.\n",
                             "\rRun function with argument allow_failure = FALSE",
                             "to see more details"))})
    
  } else {
    meteo_imgw_monthly_bp(rank,
                          year,
                          status,
                          coords,
                          station,
                          col_names,
                          ...)
  }
}

#' @noRd
#' @keywords internal
meteo_imgw_monthly_bp = function(rank,
                                 year,
                                 status,
                                 coords,
                                 station,
                                 col_names,
                                 ...) {

  translit = check_locale()
  base_url = "https://danepubliczne.imgw.pl/data/dane_pomiarowo_obserwacyjne/"
  interval_pl = "miesieczne"
  meta = meteo_metadata_imgw(interval = "monthly", rank = rank)
  rank_pl = switch(rank, synop = "synop", climate = "klimat", precip = "opad")
  # checking internet connection:
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
  catalogs = catalogs[unlist(ind)]

  all_data = vector("list", length = length(catalogs))

  for (i in seq_along(catalogs)) {
    catalog = gsub(catalogs[i], pattern = "/", replacement = "")

    if (rank == "synop") {
      address = paste0(base_url, "dane_meteorologiczne/miesieczne/synop",
                        "/", catalog, "/", catalog, "_m_s.zip")
    }
    if (rank == "climate") {
      address = paste0(base_url, "dane_meteorologiczne/miesieczne/klimat",
                        "/", catalog, "/", catalog, "_m_k.zip")
    }
    if (rank == "precip") {
      address = paste0(base_url, "dane_meteorologiczne/miesieczne/opad",
                        "/", catalog, "/", catalog, "_m_o.zip")
    }

    temp = tempfile()
    temp2 = tempfile()
    test_url(address, temp)
    #download.file(address, temp)
    unzip(zipfile = temp, exdir = temp2)
    file1 = paste(temp2, dir(temp2), sep = "/")[1]

    if (translit) {
      data1 = as.data.frame(data.table::fread(cmd = paste("iconv -f CP1250 -t ASCII//TRANSLIT", file1)))
    } else {
      data1 = suppressWarnings(read.csv(file1, header = FALSE, stringsAsFactors = FALSE, fileEncoding = "CP1250"))
    }

    colnames(data1) = meta[[1]]$parameters

    if (rank != "precip") { # w opadowkach jest tylko jeden plik
      file2 = paste(temp2, dir(temp2), sep = "/")[2]

      if (translit) {
        data2 = as.data.frame(data.table::fread(cmd = paste("iconv -f CP1250 -t ASCII//TRANSLIT", file2)))
      } else {
        data2 = read.csv(file2, header = FALSE, stringsAsFactors = FALSE, fileEncoding = "CP1250")
      }

      colnames(data2) = meta[[2]]$parameters
    }

    # removing status if set
    if (status == FALSE) {
      data1[grep("^Status", colnames(data1))] = NULL

      if (rank != "precip") { # in precipitation station only 1 file
        data2[grep("^Status", colnames(data2))] = NULL
      }
    }

    unlink(c(temp, temp2))

    if (rank != "precip") {
      all_data[[i]] = merge(data1, data2,
                             by = c("Kod stacji", "Nazwa stacji", "Rok", "Miesiac"),
                             all.x = TRUE)
    } else {
      all_data[[i]] = data1
    }
  }

  all_data = do.call(rbind, all_data)
  all_data = all_data[all_data$Rok %in% year, ]

  if (coords) {
    all_data = merge(climate::imgw_meteo_stations[, 1:3], 
                     all_data, 
                     by.x = "id", 
                     by.y = "Kod stacji", 
                     all.y = TRUE)
  }

  # add rank
  rank_code = switch(rank, synop = "SYNOPTYCZNA", climate = "KLIMATYCZNA", precip = "OPADOWA")
  all_data = cbind(data.frame(rank_code = rank_code), all_data)

  #station selection
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

  # sorting data accordingly to column names - (could be "kod stacji" or "id")
  if (sum(grepl(x = colnames(all_data), pattern = "Kod stacji"))) {
    all_data = all_data[order(all_data$`Kod stacji`, all_data$Rok, all_data$Miesiac), ]
  } else {
    all_data = all_data[order(all_data$id, all_data$Rok, all_data$Miesiac), ]
  }

  # adding option to shorten columns and removing duplicates:
  all_data = meteo_shortening_imgw(all_data, col_names = col_names, ...)
  rownames(all_data) = NULL

  return(all_data) # clipping to selected years only
}
