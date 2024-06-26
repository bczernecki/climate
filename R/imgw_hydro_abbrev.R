#' @name imgw_hydro_abbrev
#' @title Definitions of hydrological parameters used for shortening column names
#' from the danepubliczne.imgw.pl collection
#'
#' @description The object contains 3 columns that are currently used for improving
#' readability of the downloaded dataset:
#' fullname, abbr_eng, and fullname_eng
#'
#' @format The data contains a data.frame with ca. 20 elements described in three ways:
#' \describe{
#' \item{fullname}{original column names as downloaded from the repository}
#' \item{abbr_eng}{shorten column names with abbreviations derived from the most popular scheme used for meteorological parameters}
#' \item{fullname_eng}{detailed description of downloaded meteorological variables}
#' }
#' The object is created mostly to be used altogether with the hydro_shortening_imgw() function
#'
#' @docType data
#' @keywords datasets hydro abbreviations shortening
#' @examples
#' data(imgw_hydro_abbrev)
#' head(imgw_hydro_abbrev)
"imgw_hydro_abbrev"
