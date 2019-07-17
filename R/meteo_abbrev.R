#' @name meteo_abbrev
#' @title Definitions of meteorological parameters used for shortening column names
#'
#' @description The object contains 3 columns that are currently used for improving readability of the downloaded dataset:
#' fullname, abbr_eng, and fullname_eng
#'
#' @format The data contains a data.frame with ca. 250 elements described in three ways:
#' \itemize{
#'     \item{fullname} {original column names as downloaded from the repository}
#'     \item{abbr_eng} {shorten column names with abbreviations derived from the most popular scheme used for meteorological parameters}
#'     \item{fullname_eng} {detailed description of downloaded meteorological variables}
#' }
#' The object is created mostly to be used altogether with the meteo_shortening function
#'
#' @docType data
#' @keywords datasets meteo abbreviations shortening
#' @examples
#' data(meteo_abbrev)
#' head(meteo_abbrev)
"meteo_abbrev"
