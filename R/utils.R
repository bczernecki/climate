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


#' Remove columns containing status information
#' 
#' Internal function for removing columns from data frame data that contain only
#' status information and expand the created object
#' 
#' @param df data frame or data.table
#'
#' @keywords internal
#' @noRd
#' 
remove_status = function(df) {
  
  labels = sapply(df, function(x) {
    lbl = attr(x, "label")
    ifelse(is.null(lbl), NA, lbl)
  })
  
  status_cols = grepl("^Status pomiaru", labels)
  
  if (any(class(df) == "data.table")) {
    df = df[, !..status_cols]
  } else if (any(class(df) == "data.frame")) {
    df = df[, !status_cols]
  } else {
    stop("Removing status is possible only for data.frame or data.table objects")
  }
  
  return(df)
}
