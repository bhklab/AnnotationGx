#' Parse a JSON into a list
#'
#' @param response A `response` object as returned by `httr::GET`
#' @param as A `character` vector indicating the return type. Options are 'raw',
#    'text' or 'parsed'. Default is 'text'.
#' @param ... Additional arguments to the `httr::content` function.
#'
#' @seelalso [httr::content]
#'
#' @md
#' @importFrom jsonlite fromJSON
#' @importFrom httr content
#' @export
parseJSON <- function(response, ..., encoding='UTF-8', query_only=FALSE) {
    if (isTRUE(query_only)) return(response)
    tryCatch({
        fromJSON(content(response, ..., as='text', type='JSON',
            encoding=encoding))
    },
    error=function(e) {
        fromJSON(content(response, ..., type='JSON', encoding=encoding))
    })
}