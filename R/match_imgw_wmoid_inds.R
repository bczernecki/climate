#' Match WMO station IDs for IMGW SYNOP
#' @param station vector or station names provided to imgw_meteo_ family of functions
#' @keywords internal
#' @noRd
match_imgw_wmoid_inds = function(station) {
    if (is.null(station)) {
        return(NULL)
    }
    pattern_combined = paste(station, collapse = "|")
    ids_idx = grep(
        pattern = pattern_combined,
        x = climate::imgw_meteo_stations$station,
        ignore.case = TRUE
    )
    ids = climate::imgw_meteo_stations[ids_idx, "id2"]
    return(ids[nchar(ids) == 3])
}
