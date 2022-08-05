#' Check locale
#'
#' This is an extra check for some systems that make use of "C.UTF-8" or any
#' iso-like encoding recognized as causing potential problems;
#' The provided list of checked encoding cannot parse properly characters used
#' in the Polish metservice's repository and therefore will be forced to
#' use ASCII//TRANSLIT
#' @noRd

check_locale = function() {

  if (Sys.getlocale("LC_CTYPE") %in% c("C.UTF-8", "en_US.iso885915")) {
    locale = Sys.getlocale("LC_CTYPE")
    message(paste0("    Your system locale is: ", locale, " which may cause trouble.
    Please consider changing it manually while working with climate, e.g.:
    Sys.setlocale(category = 'LC_ALL', locale = 'en_US.UTF-8') "))
    Sys.sleep(4)
    return(1)
  } else {
    return(0)
  }

}
