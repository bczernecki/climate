#' @keywords internal
#' @importFrom utils packageVersion
#' @importFrom stats runif
#' @export

.onAttach = function(libname, pkgname) {
  if ((runif(1) < 0.25) & interactive()) { # activate occasionally and only if not run as Rscript
    ver = as.character(packageVersion("climate"))
    packageStartupMessage(paste0(c("\n____________________________________________________________________\n",
                                 "  Welcome to climate ", ver, "!",
                                 "\n- More about the package and datasets: bczernecki.github.io/climate",
                                 "\n- Using 'climate' for publication? See: citation('climate')\n",  
                                 "____________________________________________________________________\n")))
  }
}
