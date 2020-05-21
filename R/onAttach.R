#' @keywords internal
#' @importFrom utils packageVersion
#' @export

.onAttach <- function(libname, pkgname) {
  ver = as.character(packageVersion("climate"))
  packageStartupMessage(paste0(c("\n************************\nWelcome to climate ", ver, "!\n",
                                 "\nFind out more about the package and data sources at: github.com/bczernecki/climate",
                                 "\nUsing climate as a data source for a publication? Please cite it: citation('climate')\n",  
                                 "************************\n")))
}