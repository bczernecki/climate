#' Hydrological metadata
#'
#' Downloading the description (metadata) to hydrological data available in the dane.imgw.pl repository.
#' By default, the function returns a list or data frame for a selected subset
#`
#' @param interval temporal resolution of the data ("daily" , "monthly", or "semiannual_and_annual")
#' @keywords internal
#' 
#' @examples
#' \donttest{
#'   meta <- climate:::hydro_metadata_imgw(interval = "daily")
#'   meta <- climate:::hydro_metadata_imgw(interval = "monthly")
#'   meta <- climate:::hydro_metadata_imgw(interval = "semiannual_and_annual")
#' }

hydro_metadata_imgw <- function(interval){

  base_url <- "https://dane.imgw.pl/data/dane_pomiarowo_obserwacyjne/dane_hydrologiczne/"
  
  if (interval == "daily"){
    # dobowe
    address_meta1 <- paste0(base_url, "dobowe/codz_info.txt")
    address_meta2 <- paste0(base_url, "dobowe/zjaw_info.txt")
    meta <- list(clean_metadata_hydro(address_meta1, interval),
                 clean_metadata_hydro(address_meta2, interval))
  } else if (interval == "monthly"){
    #miesieczne
    address_meta <- paste0(base_url, "miesieczne/mies_info.txt")
    meta <- clean_metadata_hydro(address_meta, interval)
  } else if (interval == "semiannual_and_annual"){
    # polroczne_i_roczne
    address_meta <- paste0(base_url, "polroczne_i_roczne/polr_info.txt")
    meta <- clean_metadata_hydro(address_meta, interval)
  } else{
    stop("Wrong `interval` value. It should be either 'daily', 'monthly', or 'semiannual_and_annual'.")
  }

  return(meta)
}
