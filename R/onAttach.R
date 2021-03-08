#' @keywords internal
#' @importFrom utils packageVersion
#' @importFrom stats runif
#' @export

.onAttach <- function(libname, pkgname) {
  if((runif(1) < 0.2) & interactive()) { # activate occasionally and only if not run as Rscript
    ver = as.character(packageVersion("climate"))
    packageStartupMessage(paste0(c("\n______________________________________________________________\n",
                                 "  Welcome to climate ", ver, "!\n",
                                 "\n- More about the package and data sources:
				 http://github.com/bczernecki/climate",
                                 "\n- Using 'climate' for publication? See: citation('climate')\n",  
                                 "______________________________________________________________\n")))
  }
}
