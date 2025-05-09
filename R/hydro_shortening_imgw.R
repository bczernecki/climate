#' Shortening column names for hydrological variables
#'
#' Shortening column names of hydrological parameters to improve the readability of downloaded dataset from
#' the danepubliczne.imgw.pl collection and removing duplicated column names
#' @param data downloaded dataset with original column names
#' @param col_names three types of column names possible: "short" - default,
#' values with shorten names,
#' "full" - full English description,
#' "polish" - original names in the dataset
#' @param remove_duplicates whether to remove duplicated column names
#' (default TRUE - i.e., columns with duplicated names are deleted)
#' @export
#' @returns data.frame with shorten names of hydrological parameters
#' @examples
#' \donttest{
#'   monthly = data = hydro_imgw("monthly", year = 1969, col_names = "polish")
#'   
#'   if (is.data.frame(monthly)) {
#'   abbr = hydro_shortening_imgw(data = monthly,
#'       col_names = "full",
#'       remove_duplicates = TRUE)
#'   head(abbr)
#'   }
#' }
#'

hydro_shortening_imgw = function(data,
                                 col_names = "short",
                                 remove_duplicates = TRUE) {

  if (col_names != "polish") {
    abbrev = climate::imgw_hydro_abbrev
    # additional workarounds for mac os but not only...
    abbrev$fullname = gsub(x = abbrev$fullname, pattern = "'", replacement = "")
    abbrev$fullname = gsub(x = abbrev$fullname, pattern = "\\^", replacement = "")
    # end of workaround
    orig_columns = trimws(gsub("\\s+", " ", colnames(data))) # remove double spaces

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

  # removing duplicated column names:  (e.g. station's name)
  if (remove_duplicates == TRUE) {
    data = data[, !duplicated(colnames(data))]
  }
  data = unique(data)
  rownames(data) = NULL
  return(data)
}
