#' Get data from the Guide to PHARMACOLOGY Database Web Services
#' 
#' @param ids `character()` or `integer()` Identifiers to query the web
#'   service with. If excluded, the entire record for the specified service
#'   is returned.
#' @param service `character(1)` Which Guide to PHARMACOLOGY web service
#'   to query. Defaults to 'ligands'. Other options are 'targets', 'interactions',
#'   'diseases' and 'references'.
#' @param id_type `character(1)` What type of identifiers are in `ids`? Defaults
#'   to 'name', for drug name. Other options are 'accession', which accepts
#'   PubChem CIDs.
#' @param ... Force subsequent parameters to be named. Not used.
#' @param url `character(1)` The URL of the Guide to PHARMACOLOGY API. Do
#'   not change this unless you are a developer and know what you are doing.
#' 
#' @details
#' The API reference documentation can be found here:
#' https://www.guidetopharmacology.org/webServices.jsp
#' 
#' There is also a Python interface available for querying this API. See:
#' https://github.com/samirelanduk/pygtop
#' 
#' @md
#' @importFrom data.table data.table as.data.table rbindlist setnames
#' @importFrom jsonlite fromJSON
#' @importFrom httr RETRY GET status_code
#' @export
getGuideToPharm <- function(ids=NA, service='ligands', id_type='name', 
    ..., url='https://www.guidetopharmacology.org/services') 
{
    baseQuery <- .buildURL(url, service)
    if (all(is.na(ids))) {
        queries <- URLencode(baseQuery)
        queryRes <- list(RETRY(queries, 'GET', timeout(29), times=3))
    } else {
        params <- paste0(id_type, '=', ids)
        queries <- URLencode(paste0(baseQuery, '?', params))
        queryRes <- lapply(queries, RETRY, verb='GET', timeout(29), times=3)
    }
    statusCodes <- vapply(queryRes, status_code, integer(1))
    failedQueries <- statusCodes != 200
    if (any(failedQueries)) {
        failed <- Map(list, query=queries[failedQueries], 
            response=queryRes[failedQueries])
        queries <- queries[!failedQueries]
        ids <- ids[!failedQueries]
    }
    resultList <- lapply(queryRes, parseJSON)
    names(resultList) <- ids
    resultDT <- rbindlist(resultList, fill=TRUE, use.names=TRUE, idcol='query')
    if (exists('failed')) {
        attributes(resultDT)$failedQueries <- failed
    }
    return(resultDT)
}