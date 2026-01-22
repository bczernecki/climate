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
#' It accepts vector of names (characters in CAPITAL LETTERS);
#' Important: Some stations may have changed names over time in the IMGW-PIB
#' database and thus providing both names is needed
#' (e.g. `station = c("POZNAŃ", "POZNAŃ-ŁAWICA", "WARSZAWA", "WARSZAWA-OKĘCIE")`).
#' Stations' IDs (numeric) are no longer valid
#' @param col_names three types of column names possible:
#' "short" - default, values with shorten names,
#' "full" - full English description,
#' "polish" - original names in the dataset
#' @param allow_failure logical - whether to proceed or stop on failure. By default set to TRUE (i.e. don't stop on error). For debugging purposes change to FALSE
#' @param ... other parameters that may be passed to the 'shortening' function that
#' shortens column names
#' @importFrom XML readHTMLTable
#' @importFrom utils download.file unzip read.csv head
#' @importFrom data.table fread
#' @returns data.frame with a daily meteorological measurements
#' @export
#'
#' @examples \donttest{
#' daily = meteo_imgw_daily(rank = "climate", year = 2000)
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
    tryCatch(
      meteo_imgw_daily_bp(
        rank,
        year,
        status,
        coords,
        station,
        col_names
      ),
      error = function(e) {
        message(paste(
          "Potential error(s) found. Problems with downloading data.\n",
          "\rRun function with argument allow_failure = FALSE",
          "to see more details"
        ))
      }
    )
  } else {
    meteo_imgw_daily_bp(
      rank,
      year,
      status,
      coords,
      station,
      col_names,
      ...
    )
  }
}

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
  rank_pl = switch(rank,
    synop = "synop",
    climate = "klimat",
    precip = "opad"
  )

  # match WMO ID of a given station(s) to download selectively for SYNOP stations
  if (!is.null(station) && rank == "synop") {
    ids_to_download = match_imgw_wmoid_inds(station)
  }

  temp = tempfile()
  test_url(
    link = paste0(base_url, "dane_meteorologiczne/", interval_pl, "/", rank_pl, "/"),
    output = temp
  )
  a = readLines(temp, warn = FALSE)
  unlink(temp)

  ind = grep(readHTMLTable(a)[[1]]$Name, pattern = "/")
  catalogs = as.character(readHTMLTable(a)[[1]]$Name[ind])

  years_in_catalogs = strsplit(gsub(x = catalogs, pattern = "/", replacement = ""),
    split = "_"
  )
  years_in_catalogs = lapply(years_in_catalogs, function(x) x[1]:x[length(x)])
  ind = lapply(years_in_catalogs, function(x) sum(x %in% year) > 0)
  catalogs = catalogs[unlist(ind)]

  all_data = NULL

  for (i in seq_along(catalogs)) {
    catalog = gsub(catalogs[i], pattern = "/", replacement = "")

    if (rank == "synop") {
      address = paste0(
        base_url, "/dane_meteorologiczne/dobowe/synop", # nolint
        "/", catalog, "/"
      )
      test_url(link = address, output = temp)
      folder_contents = readLines(temp, warn = FALSE)
      unlink(temp)

      ind = grep(readHTMLTable(folder_contents)[[1]]$Name, pattern = "zip")
      files = as.character(readHTMLTable(folder_contents)[[1]]$Name[ind])
      addresses_to_download = paste0(address, files)

      # check against names in ids_to_download if station is not NULL:
      if (!is.null(ids_to_download)) {
        remote_files_ids = unlist(lapply(strsplit(gsub(x = basename(addresses_to_download), "_s.zip", ""), "_"), function(x) x[[length(x)]]))
        inds = which(remote_files_ids %in% ids_to_download)
        if (length(inds) > 0) {
          addresses_to_download = addresses_to_download[inds]
        }
      }

      for (j in seq_along(addresses_to_download)) {
        temp = tempfile()
        temp2 = tempfile()
        test_url(addresses_to_download[j], temp)
        invisible(unzip(zipfile = temp, exdir = temp2))
        file1 = paste(temp2, dir(temp2), sep = "/")[1]
        data1 = imgw_read(translit, file1)
        colnames(data1) = meta[[1]]$parameters
        data1$`Nazwa stacji` = trimws(data1$`Nazwa stacji`)

        file2 = paste(temp2, dir(temp2), sep = "/")[2]
        if (file.exists(file2)) {
          data2 = imgw_read(translit, file2)
          colnames(data2) = meta[[2]]$parameters
          data2$`Nazwa stacji` = trimws(data2$`Nazwa stacji`)
        } else {
          data2 = head(data1, 0)[, 1:min(5, ncol(data1))]
          data2$`Nazwa stacji` = trimws(data2$`Nazwa stacji`)
        }

        unlink(c(temp, temp2))

        # remove statuses if not needed:
        if (status == FALSE) {
          data1[grep("^Status", colnames(data1))] = NULL
          data2[grep("^Status", colnames(data2))] = NULL
        }

        ttt = merge(data1,
          data2,
          by = c("Kod stacji", "Rok", "Miesiac", "Dzien"),
          all.x = TRUE
        )

        ttt = ttt[order(ttt$`Nazwa stacji.x`, ttt$Rok, ttt$Miesiac, ttt$Dzien), ]

        if (!is.null(station)) {
          all_data[[length(all_data) + 1]] = ttt[ttt$`Nazwa stacji.x` %in% station, ]
        } else {
          all_data[[length(all_data) + 1]] = ttt
        }
      } # end of looping for zip archives
    } # end of if statement for SYNOP stations

    ######################
    ###### KLIMAT: #######
    if (rank == "climate") {
      address = paste0(
        base_url, "dane_meteorologiczne/dobowe/klimat",
        "/", catalog, "/"
      )

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
        d = tryCatch(
          expr = invisible(unzip(zipfile = temp, exdir = temp2)),
          warning = function(w) {
            env$logs = c(
              env$logs,
              paste("Warning: ", w$message, " ",
                addresses_to_download[j],
                sep = ""
              )
            )
            # try to read it with archive package:
            data = archive_read(temp, file = paste0("k_d_", sprintf("%02d", j), "_", catalog, ".csv"), format = "zip")
            csv_data = read.csv(data, header = FALSE, stringsAsFactors = FALSE, sep = ",", fileEncoding = "CP1250")
            if (!is.null(csv_data)) {
              csv_data = convert_encoding(csv_data)
              colnames(csv_data) = meta[[1]]$parameters
              csv_data$`Nazwa stacji` = trimws(csv_data$`Nazwa stacji`)
            }
            return(csv_data)
          }
        )

        if (is.data.frame(d)) {
          data1 = d
          colnames(data1) = meta[[1]]$parameters
          if (status == FALSE) {
            data1[grep("^Status", colnames(data1))] = NULL
          }
        }

        if (!is.null(d) & !is.data.frame(d)) {
          invisible(unzip(zipfile = temp, exdir = temp2))
          file1 = paste(temp2, dir(temp2), sep = "/")[1]
          data1 = imgw_read(translit, file1)
          colnames(data1) = meta[[1]]$parameters

          file2 = paste(temp2, dir(temp2), sep = "/")[2]
          if (file.exists(file2)) {
            data2 = imgw_read(translit, file2)
            colnames(data2) = meta[[2]]$parameters
          }
        }

        # remove statuses
        if (status == FALSE) {
          data1[grep("^Status", colnames(data1))] = NULL
          if (file.exists(file2)) {
            data2[grep("^Status", colnames(data2))] = NULL
          }
        }

        unlink(c(temp, temp2))
        if (file.exists(file2)) {
          all_data[[length(all_data) + 1]] = merge(data1,
            data2,
            by = c("Kod stacji", "Rok", "Miesiac", "Dzien"),
            all.x = TRUE
          )
        } else {
          all_data[[length(all_data) + 1]] = data1
        }
      } # end of looping for zip files
    } # end of if statement for climate stations

    ########################
    ######## PRECIP: #######
    if (rank == "precip") {
      address = paste0(
        base_url, "dane_meteorologiczne/dobowe/opad",
        "/", catalog, "/"
      )

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

        d = tryCatch(
          expr = invisible(unzip(zipfile = temp, exdir = temp2)),
          warning = function(w) {
            env$logs = c(
              env$logs,
              paste("Warning: ", w$message, " ",
                addresses_to_download[j],
                sep = ""
              )
            )
            # try to read it with archive package:
            data = archive_read(temp, file = paste0("o_d_", sprintf("%02d", j), "_", catalog, ".csv"), format = "zip")
            csv_data = read.table(data, header = FALSE, stringsAsFactors = FALSE, sep = ",", encoding = "CP1250")
            csv_data = convert_encoding(csv_data)
            colnames(csv_data) = meta[[1]]$parameters
            csv_data$`Nazwa stacji` = trimws(csv_data$`Nazwa stacji`)
            return(csv_data)
          }
        )

        if (is.data.frame(d)) {
          data1 = d
          colnames(data1) = meta[[1]]$parameters
          if (status == FALSE) {
            data1[grep("^Status", colnames(data1))] = NULL
          }
        }

        if (!is.null(d) & !is.data.frame(d)) {
          invisible(unzip(zipfile = temp, exdir = temp2))
          file1 = paste(temp2, dir(temp2), sep = "/")[1]
          data1 = imgw_read(translit, file1)
          colnames(data1) = meta[[1]]$parameters
          # remove status
          if (status == FALSE) {
            data1[grep("^Status", colnames(data1))] = NULL
          }
        } # end of corrupted zips
        unlink(c(temp, temp2))
        all_data[[length(all_data) + 1]] = data1
      } # end of loop for zip files
    } # end of if statement for climate stations
  } # end of looping over catalogs

  all_data = as.data.frame(data.table::rbindlist(all_data, fill = TRUE))

  # fix order of columns if needed and entries in stations' names if more than 1 available:
  col_inds = grep(pattern = "Nazwa stacji", colnames(all_data), value = TRUE)
  if (length(col_inds) > 1) {
    all_data$`Nazwa stacji` = apply(all_data[, col_inds], 1, function(x) na.omit(unique(x))[1])
    all_data$`Nazwa stacji.x` = NULL
    all_data$`Nazwa stacji.y` = NULL
    if (colnames(all_data)[ncol(all_data)] == "Nazwa stacji") { # re-order columns if needed
      all_data = all_data[, c(1, ncol(all_data), 2:(ncol(all_data) - 1))]
    }
  }

  if (coords) {
    all_data = merge(climate::imgw_meteo_stations[, 1:3],
      all_data,
      by.x = "id",
      by.y = "Kod stacji",
      all.y = TRUE
    )
  }

  # add station rank:
  rank_code = switch(rank,
    synop = "SYNOPTYCZNA",
    climate = "KLIMATYCZNA",
    precip = "OPADOWA"
  )
  all_data = cbind(data.frame(rank_code = rank_code), all_data)

  all_data = all_data[all_data$Rok %in% year, ] # clip only to selected years

  # station selection and names cleaning:
  if (!is.null(station)) {
    if (is.character(station)) {
      inds = unique(as.numeric(unlist(sapply(station, function(x) grep(pattern = x, x = trimws(all_data$`Nazwa stacji`))))))
      if (any(is.na(inds)) || length(inds) == 0) {
        env$logs = c(
          env$logs,
          paste("At least one of selected station(s) is not available in the database. Returning all available stations")
        )
      } else {
        all_data = all_data[inds, ]
      }
    }
  }
  all_data$`Nazwa stacji` = trimws(all_data$`Nazwa stacji`)

  # sort output
  if (sum(grepl(x = colnames(all_data), pattern = "Kod stacji"))) {
    all_data = all_data[order(all_data$`Kod stacji`, all_data$Rok, all_data$Miesiac, all_data$Dzien), ]
  } else {
    all_data = all_data[order(all_data$id, all_data$Rok, all_data$Miesiac, all_data$Dzien), ]
  }

  # remove duplicates and shorten colnames
  all_data = meteo_shortening_imgw(all_data, col_names = col_names, ...)
  rownames(all_data) = NULL

  # check if there any messages gathered in env$logs and if it is not empty then print them:
  if (length(env$logs) > 0) {
    message(
      "\n================================================
    \rPotential warning(s) or error(s) found.
    \rPlease carefully check content of files derived from the list below or check for the potential problems found:\n",
      paste(unique(env$logs), collapse = "\n")
    )
    env$logs = NULL
  }

  return(all_data)
}
