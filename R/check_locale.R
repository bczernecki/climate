#' Check locale
#'
#' This is an extra check for systems whose locale cannot parse properly the
#' characters used in the Polish metservice's repository. Any non-Polish locale
#' is affected, so reading is forced to use ASCII//TRANSLIT in that case.
#' @noRd
#' @keywords internal
#' @return 1 if the locale is not Polish, 0 otherwise

check_locale = function() {

  locale = Sys.getlocale("LC_CTYPE")
  if (!grepl("^pl|^polish", locale, ignore.case = TRUE)) {
    message(paste0("    Your system locale is: ", locale, " which may cause trouble.
    Please consider changing it manually while working with climate, e.g.:
    Sys.setlocale(category = 'LC_ALL', locale = 'en_US.UTF-8') "))
    Sys.sleep(4)
    return(1)
  } else {
    return(0)
  }

}
