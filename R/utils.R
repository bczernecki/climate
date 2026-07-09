#' Rename parameter code columns to full Polish label names
#'
#' Translates short internal parameter codes (e.g. NSP, POST, TMAX) to the
#' original full Polish labels stored in the metadata, so that the result can
#' be passed to `meteo_shortening_imgw()` / `hydro_shortening_imgw()`.
#' Columns not present in the metadata mapping are left unchanged.
#'
#' @param data data.frame or data.table whose column names should be renamed
#' @param meta data.frame with \code{$parameters} and \code{$label} columns,
#'   or a list of such data.frames (all are combined before matching)
#' @keywords internal
#' @importFrom stats setNames
#' @noRd
imgw_rename_params_to_labels = function(data, meta) {
  if (is.list(meta) && !is.data.frame(meta)) {
    meta = unique(do.call(rbind, meta))
  }
  param_map = setNames(meta$label, meta$parameters)
  cols = colnames(data)
  in_map = cols %in% names(param_map)
  colnames(data)[in_map] = param_map[cols[in_map]]
  data
}


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



#' Find all variants of station' names
#' 
#' For IMGW-PIB stations different naming were used historically. For example,
#' `POZNAŃ` and ``POZNAŃ-ŁAWICA, thus both names should be used when searching
#'for the station. This function finds all variants of station' names
#' status information and expand the created object
#' 
#' @param station_name character vector of station names
#' @export
#' @returns character vector of station names with all variants of station's names
#' @examples {
#' find_all_station_names(c("WARSZAWA", "POZNAŃ"))
#' }

find_all_station_names = function(station_name) {
  
  pattern = paste0("(?=.*", toupper(station_name), ")(?=.*-)")
  matches = unlist(
    sapply(pattern, function(x) {
      grep(x, climate::imgw_meteo_stations$station,
          perl = TRUE,
          ignore.case = TRUE,
          value = TRUE)
      }
    )
  )
  names(matches) = NULL

  return(sort(unique(c(station_name, matches))))
}
