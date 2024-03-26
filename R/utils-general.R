#' simple wrapper for the data.table::as.data.table() function
#' @param x object to convert to a data.table
#' @param ... additional arguments to pass to data.table::as.data.table()
#' @return a data.table
#' @keywords internal
#' @noRd
.asDT <- function(x, ...) data.table::as.data.table(x, ...)

#' Parses the query response into a data table
#'
#' This function takes a query response and converts it into a data table using the `as.data.table` function from the `data.table` package.
#'
#' @param resp The query response to be parsed
#' @return A data table containing the parsed query response
#'
#' @noRd
#' @keywords internal
.parseQueryToDT <- function(resp) {
  data.table::as.data.table(resp[[1]][[1]])
}

