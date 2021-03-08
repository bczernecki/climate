#' Meteorological metadata
#'
#' Downloading the description (metadata) to the meteorological data available in the dane.imgw repository.imgw.pl.
#' By default, the function returns a list or data frame for a selected subset
#'
#' @param interval temporal resolution of the data ("hourly", "daily", "monthly")
#' @param rank rank of station ("synop", "climate", "precip")
#' @keywords internal
#'  
#' @examples
#' \donttest{
#'   meta <- climate:::meteo_metadata_imgw(interval = "hourly", rank = "synop")
#'   meta <- climate:::meteo_metadata_imgw(interval = "daily", rank = "synop")
#'   meta <- climate:::meteo_metadata_imgw(interval = "monthly", rank = "precip")
#' }

meteo_metadata_imgw <- function(interval, rank){ # interval moze byc: monthly, hourly, hourly
  b <- NULL

  base_url <- "https://dane.imgw.pl/data/dane_pomiarowo_obserwacyjne/"
  
  # METADANE daily:
  if(interval == "daily")   { # uwaga! daily maja dla climateow i synopow po 2 pliki z metadanymi!!!

    if(rank == "synop"){
      b[[1]] <- clean_metadata_meteo(address = paste0(base_url, "dane_meteorologiczne/dobowe/synop/s_d_format.txt"),
                               rank = "synop", interval = "daily")
      b[[2]] <- clean_metadata_meteo(address = paste0(base_url, "dane_meteorologiczne/dobowe/synop/s_d_t_format.txt"),
                                              rank = "synop", interval = "daily")
    }

    if(rank == "climate"){
      b[[1]] <- clean_metadata_meteo(address = paste0(base_url, "dane_meteorologiczne/dobowe/klimat/k_d_format.txt"),
                               rank = "climate", interval = "daily")
      b[[2]] <- clean_metadata_meteo(address = paste0(base_url, "dane_meteorologiczne/dobowe/klimat/k_d_t_format.txt"),
                               rank = "climate", interval = "daily")
    }

    if(rank == "precip"){
      b[[1]] <- clean_metadata_meteo(address = paste0(base_url, "dane_meteorologiczne/dobowe/opad/o_d_format.txt"),
                               rank = "precip", interval = "daily")
    }

  }


  # TODO: pod addressem: https://dane.imgw.pl/data/dane_pomiarowo_obserwacyjne/dane_meteorologiczne/monthly/synop/
  # sa 2 ranke metadanych, bo pliki monthly maja 2 ranke danych; w starej wersji paczki tylko jedna wersja jest uwzgledniana
  # dodatkowo inne ranke danych beda do pobrania w zaleznosci od danych SYNOP, climate, precip:
  if(interval == "monthly") {

    if(rank == "synop"){
      b[[1]] <- clean_metadata_meteo(paste0(base_url, "dane_meteorologiczne/miesieczne/synop/s_m_d_format.txt"),
                               rank = "synop", interval = "monthly")
      b[[2]] <- clean_metadata_meteo(paste0(base_url, "dane_meteorologiczne/miesieczne/synop/s_m_t_format.txt"),
                               rank = "synop", interval = "monthly")
    }

    if(rank == "climate"){
      b[[1]] <- clean_metadata_meteo(paste0(base_url, "dane_meteorologiczne/miesieczne/klimat/k_m_d_format.txt"),
                               rank = "climate", interval = "monthly")
      b[[2]] <- clean_metadata_meteo(paste0(base_url, "dane_meteorologiczne/miesieczne/klimat/k_m_t_format.txt"),
                               rank = "climate", interval = "monthly")
    }

    if(rank == "precip"){
      b[[1]] <- clean_metadata_meteo(paste0(base_url, "dane_meteorologiczne/miesieczne/opad/o_m_format.txt"),
                               rank = "precip", interval = "monthly")
    }

  } # koniec MIESIECZNYCH


  ## rozpoczecie dla danych TERMINOWYCH:
  if(interval == "hourly"){
    if(rank == "synop") b[[1]] <- clean_metadata_meteo(paste0(base_url, "dane_meteorologiczne/terminowe/synop/s_t_format.txt"),
                                                 rank = "synop", interval = "hourly")
    if(rank == "climate") b[[1]] <- clean_metadata_meteo(paste0(base_url, "dane_meteorologiczne/terminowe/klimat/k_t_format.txt"),
                                                  rank = "climate", interval = "hourly")
    if(rank == "precip"){
      stop("The precipitation stations ('precip') does not provide hourly data.", call. = FALSE)
    }
  }

  return(b)
}
