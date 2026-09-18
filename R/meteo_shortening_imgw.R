#' Shortening column names for meteorological variables
#'
#' Shortening column names of meteorological parameters to improve the readability of downloaded dataset from the danepubliczne.imgw.pl collection and removing duplicated column names 
#'
#' @param data downloaded dataset with original column names
#' @param remove_duplicates whether to remove duplicated column names (default TRUE - i.e., columns with duplicated names are deleted)
#' @export
#' @returns data.frame with short English names of meteorological parameters.
#' Existing column attributes, including the original IMGW `label` metadata,
#' are preserved.
#' 
#' @examples 
#' \donttest{
#'   monthly = meteo_imgw("monthly", rank = "climate", year = 1969)
#'   
#'   abbr = meteo_shortening_imgw(data = monthly, remove_duplicates = TRUE)
#'   head(abbr)
#' }
#'

meteo_shortening_imgw = function(data, remove_duplicates = TRUE) {

  data = as.data.frame(data)
  column_attributes = lapply(data, attributes)

  # removing duplicated column names:  (e.g. station's name)
  if (remove_duplicates == TRUE) {
    keep = !duplicated(colnames(data))
    data = data[, keep, drop = FALSE]
    column_attributes = column_attributes[keep]
    # fix for merged station names with suffixes
    if (any(colnames(data) %in% c("Nazwa stacji.x", "Nazwa stacji.y"))) {
      keep = colnames(data) != "Nazwa stacji.y"
      data = data[, keep, drop = FALSE]
      column_attributes = column_attributes[keep]
      data$`Nazwa stacji.y` = NULL
      colnames(data)[colnames(data) == "Nazwa stacji.x"] = "Nazwa stacji"
    }
    
    # fix for mean air temperature which is stated sometimes in two files as:
    # "Srednia dobowa temperatura[°C]" and "Srednia temperatura dobowa [°C]"
    if (any(grepl(x = colnames(data), "Srednia dobowa temperatura"))) {
      keep = !grepl(x = colnames(data), "Srednia dobowa temperatura")
      data = data[, keep, drop = FALSE]
      column_attributes = column_attributes[keep]
    }
  }
  
  abbrev = climate::imgw_meteo_abbrev
  orig_columns = trimws(gsub("\\s+", " ", colnames(data))) # remove double spaces
  orig_columns = trimws(gsub("\\[.*?]", "", orig_columns)) # remove brackets and content inside

  abbrev$fullname = trimws(gsub("\\[.*?]", "", abbrev$fullname))
  matches = match(orig_columns, abbrev$fullname)
  matches = matches[!is.na(matches)]
  colnames(data)[orig_columns %in% abbrev$fullname] = abbrev$abbr_eng[matches]
  data = unique(data)
  for (ind in seq_along(column_attributes)) {
    if (!is.null(column_attributes[[ind]])) {
      attributes(data[[ind]]) = column_attributes[[ind]]
    }
  }
  rownames(data) = NULL
  return(data)
}
