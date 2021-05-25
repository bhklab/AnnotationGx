#' An API Connection Class
#'
#' A class representing a connection to an API at a specific URL. Stores
#'   and automatically retrieves a set of metadata associated with the API
#'   including any documentation, if available, the types of requests it accepts
#'   and what targets it exposes for queries.
#'
#' @slot url `character`
#' @slot documentation `list`
#' @slot accepts `character`
#' @slot targets `character`
#'
#'
#' @md
#' @keywords internal
.API <- setClass('API',
                 representation=list(
                     url='character',
                     documentation='list',
                     accepts='character',
                     targets='list'
                 ))

#' Constructor for an API Connection Class
#'
#' @params url `character` the url of the API you wish to connect to
#'
#' @return an `API` object storing the API connection and metadata about it.
#'
#' @import httr
#' @import jsonlite
#' @import xml2
#' @export
API <- function(url) {
    .API(url, documentation=list(), accepts='', targets=list())
}