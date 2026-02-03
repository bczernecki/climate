#' Environment to log unique entries related with downloading problems
#' @return Empty env
#' @keywords internal
#' @noRd
env <- new.env(parent = emptyenv())

globalVariables(c("DZ", "GG", "MC", "NSP", "POST.x", "ROK", "id",
                  "..status_cols", "status_cols"))