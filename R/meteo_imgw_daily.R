#' Daily IMGW meteorological data
#'
#' Downloading daily (meteorological) data from the SYNOP / CLIMATE / PRECIP stations available in the dane.imgw.pl collection
#'
#' @param rank rank of the stations: "synop" (default), "climate", or "precip"
#' @param year vector of years (e.g., 1966:2000)
#' @param status leave the columns with measurement and observation statuses (default status = FALSE - i.e. the status columns are deleted)
#' @param coords add coordinates of the station (logical value TRUE or FALSE)
#' @param station name of meteorological station(s).
#' It accepts names (characters in CAPITAL LETTERS); stations' IDs (numeric) are no longer valid
#' @param col_names three types of column names possible: "short" - default, values with shorten names, "full" - full English description, "polish" - original names in the dataset
#' @param ... other parameters that may be passed to the 'shortening' function that shortens column names
#' @importFrom XML readHTMLTable
#' @importFrom utils download.file unzip read.csv
#' @export
#'
#' @examples \donttest{
#'   daily <- meteo_imgw_daily(rank = "climate", year = 2000)
#'   head(daily)
#' }
#'

meteo_imgw_daily <- function(rank = "synop", year, status = FALSE, coords = FALSE, station = NULL, col_names = "short", ...){

  #options(RCurlOptions = list(ssl.verifypeer = FALSE)) # required on windows for RCurl
  
  base_url <- "https://dane.imgw.pl/data/dane_pomiarowo_obserwacyjne/"
  
  interval <- "daily" # to mozemy ustawic na sztywno
  interval_pl <- "dobowe"
  meta = meteo_metadata_imgw(interval = "daily", rank = rank)
  
  rank_pl <- switch(rank, synop = "synop", climate = "klimat", precip = "opad")

  
  temp = tempfile()
  test_url(link = paste0(base_url, "dane_meteorologiczne/", interval_pl, "/", rank_pl, "/"),
          output = temp)
  a = readLines(temp, warn = FALSE)
  unlink(temp)
  
  # a = getURL(paste0(base_url, "dane_meteorologiczne/", interval_pl, "/", rank_pl, "/"),
  #            ftp.use.epsv = FALSE,
  #            dirlistonly = TRUE)
  
  ind <- grep(readHTMLTable(a)[[1]]$Name, pattern = "/")
  catalogs <- as.character(readHTMLTable(a)[[1]]$Name[ind])
  
  # fragment dla lat (ktore catalogs wymagaja pobrania:
  years_in_catalogs <- strsplit(gsub(x = catalogs, pattern = "/", replacement = ""), split = "_")
  years_in_catalogs <- lapply(years_in_catalogs, function(x) x[1]:x[length(x)])
  ind <- lapply(years_in_catalogs, function(x) sum(x %in% year) > 0)
  catalogs <- catalogs[unlist(ind)] # to sa nasze prawdziwe catalogs do przemielenia
  
  all_data <- NULL
  
  for (i in seq_along(catalogs)){
    catalog <- gsub(catalogs[i], pattern = "/", replacement = "")
    
    if(rank == "synop") {
      address <- paste0(base_url, "/dane_meteorologiczne/dobowe/synop",
                        "/", catalog, "/")
      #folder_contents <- getURL(address, ftp.use.epsv = FALSE, dirlistonly = FALSE) # zawartosc folderu dla wybranego yearu
      
      test_url(link = address, output = temp)
      folder_contents = readLines(temp, warn = FALSE)
      unlink(temp)
      
      ind <- grep(readHTMLTable(folder_contents)[[1]]$Name, pattern = "zip")
      files <- as.character(readHTMLTable(folder_contents)[[1]]$Name[ind])
      addresses_to_download <- paste0(address, files)
      # w tym miejscu trzeba przemyslec fragment kodu do dodania dla pojedynczej stacji jesli tak sobie zazyczy uzytkownik:
      # na podstawie zawartosci obiektu files
      
      for(j in seq_along(addresses_to_download)){
        temp <- tempfile()
        temp2 <- tempfile()
        test_url(addresses_to_download[j], temp)
        #download.file(addresses_to_download[j], temp)
        unzip(zipfile = temp, exdir = temp2)
        file1 <- paste(temp2, dir(temp2), sep = "/")[1]
        data1 <- read.csv(file1, header = FALSE, stringsAsFactors = FALSE, fileEncoding = "CP1250")
        colnames(data1) <- meta[[1]]$parameters
        
        file2 <- paste(temp2, dir(temp2), sep = "/")[2]
        data2 <- read.csv(file2, header = FALSE, stringsAsFactors = FALSE, fileEncoding = "CP1250")
        colnames(data2) <- meta[[2]]$parameters
        
        # usuwa statusy
        if(status == FALSE){
          data1[grep("^Status", colnames(data1))] <- NULL
          data2[grep("^Status", colnames(data2))] <- NULL
        }
        
        unlink(c(temp, temp2))
        
        # moja proba z obejsciem dla wyboru kodu
        ttt = merge(data1, data2, by = c("Kod stacji",  "Rok", "Miesiac", "Dzien"), all.x = TRUE)
        ttt = ttt[order(ttt$`Nazwa stacji.x`, ttt$Rok, ttt$Miesiac, ttt$Dzien),]
        ### ta część kodu powtarza sie po dużej petli od rank
        if (!is.null(station)) {
          all_data[[length(all_data) + 1]] = ttt[substr(ttt$`Nazwa stacji.x`,1,nchar(station))==station,]
        } else {
          all_data[[length(all_data) + 1]] <- ttt
        }
        # koniec proby z obejsciem
        
      } # koniec petli po zipach do pobrania
      
    } # koniec if'a dla synopa
    
    ######################
    ###### KLIMAT: #######
    if(rank == "climate") {
      address <- paste0(base_url, "dane_meteorologiczne/dobowe/klimat",
                        "/", catalog, "/")
      #folder_contents <- getURL(address, ftp.use.epsv = FALSE, dirlistonly = FALSE) # zawartosc folderu dla wybranego yearu
      
      test_url(link = address, output = temp)
      folder_contents = readLines(temp, warn = FALSE)
      unlink(temp)
      
      ind <- grep(readHTMLTable(folder_contents)[[1]]$Name, pattern = "zip")
      files <- as.character(readHTMLTable(folder_contents)[[1]]$Name[ind])
      addresses_to_download <- paste0(address, files)
      # w tym miejscu trzeba przemyslec fragment kodu do dodania dla pojedynczej stacji jesli tak sobie zazyczy uzytkownik:
      # na podstawie zawartosci obiektu files
      
      for(j in seq_along(addresses_to_download)){
        temp <- tempfile()
        temp2 <- tempfile()
        test_url(addresses_to_download[j], temp)
        unzip(zipfile = temp, exdir = temp2)
        file1 <- paste(temp2, dir(temp2), sep = "/")[1]
        data1 <- read.csv(file1, header = FALSE, stringsAsFactors = FALSE, fileEncoding = "CP1250")
        colnames(data1) <- meta[[1]]$parameters
        
        file2 <- paste(temp2, dir(temp2), sep = "/")[2]
        data2 <- read.csv(file2, header = FALSE, stringsAsFactors = FALSE, fileEncoding = "CP1250")
        colnames(data2) <- meta[[2]]$parameters
        
        # usuwa statusy
        if(status == FALSE){
          data1[grep("^Status", colnames(data1))] <- NULL
          data2[grep("^Status", colnames(data2))] <- NULL
        }
        
        unlink(c(temp, temp2))
        all_data[[length(all_data)+1]] <- merge(data1, data2,
                                                by = c("Kod stacji", "Rok", "Miesiac", "Dzien"),
                                                all.x = TRUE)
      } # koniec petli po zipach do pobrania
    } # koniec if'a dla klimatu
    
    
    
    ######################
    ######## OPAD: #######
    if(rank == "precip") {
      address <- paste0(base_url, "dane_meteorologiczne/dobowe/opad",
                        "/", catalog, "/")
      #folder_contents <- getURL(address, ftp.use.epsv = FALSE, dirlistonly = FALSE) # zawartosc folderu dla wybranego yearu
      
      test_url(link = address, output = temp)
      folder_contents = readLines(temp, warn = FALSE)
      unlink(temp)
      
      ind <- grep(readHTMLTable(folder_contents)[[1]]$Name, pattern = "zip")
      files <- as.character(readHTMLTable(folder_contents)[[1]]$Name[ind])
      addresses_to_download <- paste0(address, files)
      
      for(j in seq_along(addresses_to_download)){
        temp <- tempfile()
        temp2 <- tempfile()
        test_url(addresses_to_download[j], temp)
        unzip(zipfile = temp, exdir = temp2)
        file1 <- paste(temp2, dir(temp2), sep = "/")[1]
        data1 <- read.csv(file1, header = FALSE, stringsAsFactors = FALSE, fileEncoding = "CP1250")
        colnames(data1) <- meta[[1]]$parameters
        # usuwa statusy
        if(status == FALSE){
          data1[grep("^Status", colnames(data1))] <- NULL
        }
        
        unlink(c(temp, temp2))
        all_data[[length(all_data)+1]] <- data1
      } # koniec petli po zipach do pobrania
    } # koniec if'a dla klimatu
    
    
  } # koniec petli po glownych catalogach danych dobowych
  
  all_data <- do.call(rbind, all_data)
  
  if (coords){
    all_data <- merge(climate::imgw_meteo_stations, all_data, by.x = "id", by.y = "Kod stacji", all.y = TRUE)
  }
  
  # dodaje rank
  rank_code <- switch(rank, synop = "SYNOPTYCZNA", climate = "KLIMATYCZNA", precip = "OPADOWA")
  all_data <- cbind(data.frame(rank_code = rank_code), all_data)
  
  all_data <- all_data[all_data$Rok %in% year, ] # przyciecie tylko do wybranych lat gdyby sie pobralo za duzo
  
  #station selection
  if (!is.null(station)) {
    if (is.character(station)) {
      
      if(rank == 'synop' | rank == 'climate') all_data <- all_data[substr(all_data$`Nazwa stacji.x`,1,nchar(station))==station, ] # sprawdzic tutaj czy jest Nazwa stacji.x w synopach
      
      # exception for column names in precipitation data:                                                
      if(rank == 'precip') all_data <- all_data[substr(all_data$`Nazwa stacji`,1,nchar(station))==station, ] # sprawdzic tutaj czy jest Nazwa stacji.x w synopach
      
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
    all_data <- all_data[order(all_data$`Kod stacji`, all_data$Rok, all_data$Miesiac, all_data$Dzien), ]
  } else {
    all_data <- all_data[order(all_data$id, all_data$Rok, all_data$Miesiac, all_data$Dzien), ]
  }

  
  
  # # dodanie opcji  dla skracania kolumn i usuwania duplikatow:
  all_data <- meteo_shortening_imgw(all_data, col_names = col_names, ...)
  
  return(all_data)
  

} # koniec funkcji meteo_daily
