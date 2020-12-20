#' Monthly IMGW meteorological data
#'
#' Downloading monthly (meteorological) data from the SYNOP / CLIMATE / PRECIP stations available in the dane.imgw.pl collection
#'
#' @param rank rank of the stations: "synop" (default), "climate", or "precip"
#' @param year vector of years (e.g., 1966:2000)
#' @param status leave the columns with measurement and observation statuses (default status = FALSE - i.e. the status columns are deleted)
#' @param coords add coordinates of the station (logical value TRUE or FALSE)
#' @param station name or ID of meteorological station(s).
#' It accepts names (characters in CAPITAL LETTERS) or stations' IDs (numeric)
#' @param col_names three types of column names possible: "short" - default, values with shorten names, "full" - full English description, "polish" - original names in the dataset
#' @param ... other parameters that may be passed to the 'shortening' function that shortens column names
#' @importFrom XML readHTMLTable
#' @importFrom utils unzip read.csv
#' @export
#'
#' @examples \donttest{
#'   monthly <- meteo_imgw_monthly(rank = "climate", year = 1969)
#'   head(monthly)
#'
#'   # a descriptive (long) column names:
#'   monthly2 <- meteo_imgw_monthly(rank = "synop", year = 2018, 
#'          col_names = "full")
#'   head(monthly2)
#'   
#'   # please note that station names may change over time 
#'   # and thus 2 names are required in some cases:
#'   df = meteo_imgw_monthly(rank = 'synop', year = 1991:2000, 
#'           coords = TRUE, station = c("POZNAŃ","POZNAŃ-ŁAWICA")) 
#' }
#'

meteo_imgw_monthly <- function(rank = "synop", year, status = FALSE, coords = FALSE, station = NULL, col_names = "short", ...){

  #options(RCurlOptions = list(ssl.verifypeer = FALSE)) # required on windows for RCurl
  
  base_url <- "https://dane.imgw.pl/data/dane_pomiarowo_obserwacyjne/"
  
  # if (httr::http_error(base_url)) {
  #   b = stop(call. = FALSE, 
  #            paste0("\nDownload failed. ",
  #                   "Check your internet connection or validate this url in your browser: ",
  #                   base_url,
  #                   "\n"))
  # }
  # 
  # 
  interval_pl <- "miesieczne" # to mozemy ustawic na sztywno do odwolania w url
  meta <- meteo_metadata_imgw(interval = "monthly", rank = rank)
  
  rank_pl <- switch(rank, synop = "synop", climate = "klimat", precip = "opad")
  
  # checking net connection:
  temp = tempfile()
  test_url(link = paste0(base_url, "dane_meteorologiczne/", interval_pl, "/", rank_pl, "/"),
           output = temp)
  a = readLines(temp, warn = FALSE)
  unlink(temp)
  
  # a <- getURL(paste0(base_url, "dane_meteorologiczne/", interval, "/", rank_pl, "/"),
  #             ftp.use.epsv = FALSE,
  #             dirlistonly = TRUE)
  ind <- grep(readHTMLTable(a)[[1]]$Name, pattern = "/")
  catalogs <- as.character(readHTMLTable(a)[[1]]$Name[ind])
  
  # fragment dla lat (ktore catalogs wymagaja pobrania:
  years_in_catalogs <- strsplit(gsub(x = catalogs, pattern = "/", replacement = ""), split = "_")
  years_in_catalogs <- lapply(years_in_catalogs, function(x) x[1]:x[length(x)])
  ind <- lapply(years_in_catalogs, function(x) sum(x %in% year) > 0)
  catalogs <- catalogs[unlist(ind)] # to sa nasze prawdziwe catalogs do przemielenia
  
  all_data <- vector("list", length = length(catalogs))
  
  for (i in seq_along(catalogs)){
    # print(i)
    catalog <- gsub(catalogs[i], pattern = "/", replacement = "")
    
    if(rank == "synop") {
      address <- paste0(base_url, "dane_meteorologiczne/miesieczne/synop",
                        "/", catalog, "/", catalog, "_m_s.zip")
    }
    if(rank == "climate") {
      address <- paste0(base_url, "dane_meteorologiczne/miesieczne/klimat",
                        "/", catalog, "/", catalog, "_m_k.zip")
    }
    if(rank == "precip") {
      address <- paste0(base_url, "dane_meteorologiczne/miesieczne/opad",
                        "/", catalog, "/", catalog, "_m_o.zip")
    }
    
    temp <- tempfile()
    temp2 <- tempfile()
    test_url(address, temp)
    #download.file(address, temp)
    unzip(zipfile = temp, exdir = temp2)
    file1 <- paste(temp2, dir(temp2), sep = "/")[1]
    data1 <- read.csv(file1, header = FALSE, stringsAsFactors = FALSE, fileEncoding = "CP1250")
    colnames(data1) <- meta[[1]]$parameters
    
    if( rank != "precip"){ # w opadowkach jest tylko jeden plik
      file2 <- paste(temp2, dir(temp2), sep = "/")[2]
      data2 <- read.csv(file2, header = FALSE, stringsAsFactors = FALSE, fileEncoding = "CP1250")
      colnames(data2) <- meta[[2]]$parameters
    }
    
    # usuwa statusy
    if(status == FALSE){
      data1[grep("^Status", colnames(data1))] <- NULL
      
      if(rank != "precip"){ # w plikach opadowych tylko jeden plik
        data2[grep("^Status", colnames(data2))] <- NULL
      }
    }
    
    unlink(c(temp, temp2))
    
    if(rank != "precip"){
      all_data[[i]] <- merge(data1, data2,
                             by = c("Kod stacji", "Nazwa stacji", "Rok", "Miesiac"),
                             all.x = TRUE)
    } else {
      all_data[[i]] <- data1
    }
  }
  
  all_data <- do.call(rbind, all_data)
  all_data <- all_data[all_data$Rok %in% year, ]
  
  if (coords){
    all_data <- merge(climate::imgw_meteo_stations, all_data, by.x = "id", by.y = "Kod stacji", all.y = TRUE)
  }
  
  # dodaje rank
  rank_code <- switch(rank, synop = "SYNOPTYCZNA", climate = "KLIMATYCZNA", precip = "OPADOWA")
  all_data <- cbind(data.frame(rank_code = rank_code), all_data)
  
  #station selection
  if (!is.null(station)) {
    if (is.character(station)) {
      all_data <- all_data[substr(all_data$`Nazwa stacji`,1,nchar(station))==station, ]
      if (nrow(all_data) == 0){
        stop("Selected station(s) is not available in the database.", call. = FALSE)
      }
    } else if (is.numeric(station)){
      all_data <- all_data[all_data$`Kod stacji` %in% station, ]
      if (nrow(all_data) == 0){
        stop("Selected station(s) is not available in the database.", call. = FALSE)
      }
    } else {
      stop("Selected station(s) are not in the proper format.", call. = FALSE)
    }
  }
  
  
  # sortowanie w zaleznosci od nazw kolumn - raz jest "kod stacji", raz "id"
  if(sum(grepl(x = colnames(all_data), pattern = "Kod stacji"))){
    all_data <- all_data[order(all_data$`Kod stacji`, all_data$Rok, all_data$Miesiac), ]
  } else {
    all_data <- all_data[order(all_data$id, all_data$Rok, all_data$Miesiac), ]
  }
  
  
  # dodanie opcji  dla skracania kolumn i usuwania duplikatow:
  all_data <- meteo_shortening_imgw(all_data, col_names = col_names, ...)
  
  return(all_data) # przyciecie tylko do wybranych lat gdyby sie pobralo za duzo
}
