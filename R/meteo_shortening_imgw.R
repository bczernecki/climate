#' Shortening column names for meteorological variables
#'
#' Shortening column names of meteorological parameters to improve the readability of downloaded dataset from the danepubliczne.imgw.pl collection and removing duplicated column names 
#'
#' @param data downloaded dataset with original column names
#' @param col_names three types of column names possible: "short" - default, values with shorten names, "full" - full English description, "polish" - original names in the dataset
#' @param remove_duplicates whether to remove duplicated column names (default TRUE - i.e., columns with duplicated names are deleted)
#' @export
#' @returns data.frame with modified names of meteorological parameters
#' 
#' @examples 
#' \donttest{
#'   monthly = meteo_imgw("monthly", rank = "climate", year = 1969)
#'   
#'   abbr = meteo_shortening_imgw(data = monthly,
#'       col_names = "full", 
#'       remove_duplicates = TRUE)
#'   head(abbr)
#' }
#'

meteo_shortening_imgw = function(data, col_names = "short", remove_duplicates = TRUE) {

  # removing duplicated column names:  (e.g. station's name)
  if (remove_duplicates == TRUE) {
    data = data[, !duplicated(colnames(data))]
    # fix for merged station names with suffixes
    if (any(colnames(data) %in% c("Nazwa stacji.x", "Nazwa stacji.y"))) {
      data$`Nazwa stacji.y` = NULL
      colnames(data)[colnames(data) == "Nazwa stacji.x"] = "Nazwa stacji"
    }
    
    # fix for mean air temperature which is stated sometimes in two files as:
    # "Srednia dobowa temperatura[°C]" and "Srednia temperatura dobowa [°C]"
    if (any(grepl(x = colnames(data), "Srednia dobowa temperatura"))) {
      data[, which(grepl(x = colnames(data), "Srednia dobowa temperatura"))] = NULL
    }
  }
  
  if (col_names != "polish") {
    abbrev = climate::imgw_meteo_abbrev
    orig_columns = trimws(gsub("\\s+", " ", colnames(data))) # remove double spaces
    orig_columns = trimws(gsub("\\[.*?]", "", orig_columns)) # remove brackets and content inside

    abbrev$fullname = trimws(gsub("\\[.*?]", "", abbrev$fullname))
    matches = match(orig_columns, abbrev$fullname)
    matches = matches[!is.na(matches)]

    if (col_names == "short") {
      # abbrev english
      colnames(data)[orig_columns %in% abbrev$fullname] = abbrev$abbr_eng[matches]
    }

    if (col_names == "full") {
      # full english names:
      colnames(data)[orig_columns %in% abbrev$fullname] = abbrev$fullname_eng[matches]
    }
  }
  data = unique(data)
  rownames(data) = NULL
  return(data)
}
