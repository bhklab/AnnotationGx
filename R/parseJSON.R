

#' Parse a JSON into a list
#'
#' @param response A `response` object as returned by `httr::GET`
#' @param as A `character` vector indicating the return type. Options are 'raw',
#    'text' or 'parsed'. Default is 'text'.
#' @param ... Additional arguments to the `httr::content` function.
#'
#' @seealso [httr::content]
#'
#' @md
#' @importFrom jsonlite fromJSON
#' @importFrom httr content
#' @export 
parseJSON <- function(response, ..., encoding='UTF-8', query_only=FALSE) {
    if (isTRUE(query_only)) return(response)
    response <- content(response, encoding = "UTF-8", as='text', type='JSON')

    # if (is.null(response)) return(NA)
    # if (is.na(response)) return(NA)

    if (is.null(response) | is.na(response)) return(NULL)

    tryCatch({
        fromJSON(response, ...)
    },
    error=function(e) {
        NA
    })
}
