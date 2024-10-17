#' Meteorological metadata
#'
#' Downloading the description (metadata) to the meteorological data available in the 
#' danepubliczne.imgw.pl collection.
#' By default, the function returns a list or data frame for a selected subset
#'
#' @param interval temporal resolution of the data ("hourly", "daily", "monthly")
#' @param rank rank of station ("synop", "climate", "precip")
#' @keywords internal
#' @noRd

meteo_metadata_imgw = function(interval, rank) { # interval can be: monthly, hourly, hourly
  
  b = NULL
  base_url = "https://danepubliczne.imgw.pl/data/dane_pomiarowo_obserwacyjne/"

  # METADATA daily:
  if (interval == "daily") { # warning! daily for climates and synop have 2 files with metadata!!!

    if (rank == "synop") {
      b[[1]] = clean_metadata_meteo(address = paste0(base_url,"dane_meteorologiczne/dobowe/synop/s_d_format.txt"),
                               rank = "synop", interval = "daily")
      b[[2]] = clean_metadata_meteo(address = paste0(base_url, "dane_meteorologiczne/dobowe/synop/s_d_t_format.txt"),
                                              rank = "synop", interval = "daily")
    }

    if (rank == "climate") {
      b[[1]] = clean_metadata_meteo(address = paste0(base_url, "dane_meteorologiczne/dobowe/klimat/k_d_format.txt"),
                               rank = "climate", interval = "daily")
      b[[2]] = clean_metadata_meteo(address = paste0(base_url, "dane_meteorologiczne/dobowe/klimat/k_d_t_format.txt"),
                               rank = "climate", interval = "daily")
    }

    if (rank == "precip") {
      b[[1]] = clean_metadata_meteo(address = paste0(base_url, "dane_meteorologiczne/dobowe/opad/o_d_format.txt"),
                               rank = "precip", interval = "daily")
    }

  } # end of daily interval

  if (interval == "monthly") {

    if (rank == "synop") {
      b[[1]] = clean_metadata_meteo(paste0(base_url, "dane_meteorologiczne/miesieczne/synop/s_m_d_format.txt"),
                               rank = "synop", interval = "monthly")
      b[[2]] = clean_metadata_meteo(paste0(base_url, "dane_meteorologiczne/miesieczne/synop/s_m_t_format.txt"),
                               rank = "synop", interval = "monthly")
    }

    if (rank == "climate") {
      b[[1]] = clean_metadata_meteo(paste0(base_url, "dane_meteorologiczne/miesieczne/klimat/k_m_d_format.txt"),
                               rank = "climate", interval = "monthly")
      b[[2]] = clean_metadata_meteo(paste0(base_url, "dane_meteorologiczne/miesieczne/klimat/k_m_t_format.txt"),
                               rank = "climate", interval = "monthly")
    }

    if (rank == "precip") {
      b[[1]] = clean_metadata_meteo(paste0(base_url, "dane_meteorologiczne/miesieczne/opad/o_m_format.txt"),
                               rank = "precip", interval = "monthly")
    }

  } # end of monthly interval

  ## hourly data section:
  if (interval == "hourly") {
    if (rank == "synop") b[[1]] = clean_metadata_meteo(paste0(base_url, "dane_meteorologiczne/terminowe/synop/s_t_format.txt"),
                                                 rank = "synop", interval = "hourly")
    if (rank == "climate") b[[1]] = clean_metadata_meteo(paste0(base_url, "dane_meteorologiczne/terminowe/klimat/k_t_format.txt"),
                                                  rank = "climate", interval = "hourly")
    if (rank == "precip") {
      stop("The precipitation stations ('precip') does not provide hourly data.", call. = FALSE)
    }
  }
  return(b)
}
