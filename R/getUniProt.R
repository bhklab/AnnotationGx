#'
#'
#'
#'
#'
#'
#'
getUniProt <- function(url='') {
    message("not implemented yet :(")
}

#' Query the UniProt website REST API
#'
#' @param query A `character` vector of the API query to return. See
#'   https://www.uniprot.org/help/text-search for information on query
#'   structure.
#' @param format A `character` vector specifying the return format. Options
#'   are 'html', 'tab', 'xls', 'fasta', 'gff', 'txt', 'xml', 'rdf', 'list' or
#'   'rss'. Default is 'xml'. See https://www.uniprot.org/help/api%5Fqueries
#'   for more information.
#' @param include A `boolean` indicating if isoforms or description of
#'   referenced data should be included.
#' @param compress TODO::
#' @param ... Catch all unnamed arguments. All parameters after ... must be
#'   specified with as name=value or they will be ignored.
#' @param url A `character` vector of the URL for the REST API. Should not
#'   include trailing '/', this will be added inside the function. This
#'   parameter must be passed named or it will not work.
#'
#' @return A httr::response object containing the query results as `format`.
#'
#' @import httr
#' @importFrom CoreGx .errorMsg .warnMsg
# #' @export
queryUniProt <- function(query, columns='', limit='', offset='', format='xml',
        include=TRUE, compress=TRUE, ...,
        url='https://www.uniprot.org/uniprot') {
    if (missing(query)) stop(.errorMsg(.context(), 'The query parameter is
        missing! This parameter is mandatory.'))

    ##TODO:: handle more errors

    # parse parameters
    include <- if (include) 'yes' else 'no'
    compress <- if (compress) 'yes' else 'no'

    response <- GET()
}

#'
#'
#'
#'
