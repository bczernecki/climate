#' Clean mis-encoded columns in data frame
#'
#' Internal function for cleaning mis-encoded characters in non-typical IMGW files
#' @param df data frame
#'
#' @keywords internal
#' @noRd

convert_encoding = function(df) {
  df[] = lapply(df, function(col) {
    if (is.character(col)) {
      iconv(col, from = "CP1250", to = "UTF-8")
    } else if (is.factor(col)) {
      factor(iconv(as.character(col), from = "CP1250", to = "UTF-8"))
    } else {
      col
    }
  })
  return(df)
}
