#' Check locale
#'
#' This is an extra check for some systems that make use of "C.UTF-8" that cannot parse properly tags used inside the Polish metservice's repository
#' @noRd

check_locale = function(){
  if(any(strsplit(Sys.getlocale(), "/")[[1]] == "C.UTF-8")){
    message("  Your system locale contains 'C.UTF-8' which may cause trouble. 
    Please consider changing it manually while working with climate, e.g.: 
    Sys.setlocale(category = 'LC_ALL', locale = 'en_US.UTF-8') ")
  }
}
