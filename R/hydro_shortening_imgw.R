#' Shortening column names for hydrological variables
#'
#' Shortening column names of hydrological parameters to improve the readability of downloaded dataset from
#' the danepubliczne.imgw.pl collection and removing duplicated column names
#' @param data downloaded dataset with original column names
#' @param remove_duplicates whether to remove duplicated column names
#' (default TRUE - i.e., columns with duplicated names are deleted)
#' @export
#' @returns data.frame with short English names of hydrological parameters.
#' Existing column attributes, including the original IMGW `label` metadata,
#' are preserved.
#' @examples
#' \donttest{
#'   monthly = data = hydro_imgw("monthly", year = 1969)
#'   
#'   if (is.data.frame(monthly)) {
#'   abbr = hydro_shortening_imgw(data = monthly, remove_duplicates = TRUE)
#'   head(abbr)
#'   }
#' }
#'

hydro_shortening_imgw = function(data,
                                 remove_duplicates = TRUE) {

  data = as.data.frame(data)
  column_attributes = lapply(data, attributes)

  abbrev = climate::imgw_hydro_abbrev
  # additional workarounds for mac os but not only...
  abbrev$fullname = gsub(x = abbrev$fullname, pattern = "'", replacement = "")
  abbrev$fullname = gsub(x = abbrev$fullname, pattern = "\\^", replacement = "")
  abbrev$fullname = stringi::stri_trans_general(abbrev$fullname, "LATIN-ASCII")
  # end of workaround
  orig_columns = trimws(gsub("\\s+", " ", colnames(data))) # remove double spaces
  orig_columns = gsub(x = orig_columns, pattern = "'", replacement = "")
  orig_columns = gsub(x = orig_columns, pattern = "\\^", replacement = "")
  orig_columns = stringi::stri_trans_general(orig_columns, "LATIN-ASCII")
  # `Data` is created by the package and is not an IMGW parameter.
  orig_columns[orig_columns == "Data"] = NA_character_

  matches = match(orig_columns, abbrev$fullname)
  matches = matches[!is.na(matches)]
  colnames(data)[orig_columns %in% abbrev$fullname] = abbrev$abbr_eng[matches]

  # removing duplicated column names:  (e.g. station's name)
  if (remove_duplicates == TRUE) {
    keep = !duplicated(colnames(data))
    data = data[, keep, drop = FALSE]
    column_attributes = column_attributes[keep]
  }
  data = unique(data)
  for (ind in seq_along(column_attributes)) {
    if (!is.null(column_attributes[[ind]])) {
      attributes(data[[ind]]) = column_attributes[[ind]]
    }
  }
  rownames(data) = NULL
  return(data)
}
