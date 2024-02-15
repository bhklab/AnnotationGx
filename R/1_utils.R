
#' simple wrapper for the data.table::as.data.table() function
#' @param x object to convert to a data.table
#' @param ... additional arguments to pass to data.table::as.data.table()
#' @return a data.table
#' @keywords internal
#' @noRd
.asDT <- function(x, ...) data.table::as.data.table(x, ...)


