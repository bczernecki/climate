#' @keywords internal
#' @export

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0(c("\n************************\nWelcome to climate ", as.character(packageVersion("climate")), "!\n",
                                 "Find out more about the package and data sources at: github.com/bczernecki/climate\n",
                                 "Using climate as a data source for a publication? Please cite it: citation('climate')\n",  
                                 "************************\n")))
}