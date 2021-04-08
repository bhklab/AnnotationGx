#' Fetch 
#'
#'
#'
#' @importFrom httr GET
#' @import jsonlite
#' @import data.table
getPubChem <- function(id, input='compound', identifier='cid', operation='',
    output='JSON', ..., url='https://pubchem.ncbi.nlm.nih.gov/rest/pug',
    filter='')

{
    # handle list or vector inputs for id
    if (length(id) > 1) id <- paste0(na.omit(id), collapse=',')

    # build query URL
    query <- .buildURL(url, input, identifier, id, operation, output)
    query <- paste(query, filter, sep='?')
    print(query)

    # get HTTP response
    GET(query)
}

#' Parse a JSON into a list
#' 
#' @param response A `response` object as returned by `httr::GET`
#' @param as A `character` vector indicating the return type. Options are 'raw', 'text' or
#'  'parsed'. Default is 'text'.
#' @param ... Additional arguments to the `httr::content` function. 
#' 
#' @seelalso httr::content
#' 
#' @importFrom jsonlite fromJSON
#' @importFrom httr content
#' @md
#' @export
parseJSON <- function(response, as='text', ...) {
    fromJSON(content(response, as, ...))
}

#' Query the PubChem REST API, with the result automatically converted from
#'  JSON to a list.
#' 
#' @param ... Fallthrough arguments to `AnnotationGx::getPubChem` function.
#' 
#' @md
#' @export
queryPubChem <- function(...) parseJSON(getPubChem(...))

#' Build a `data.table` of assay ids from the a PubChem query list.
#' 
#' @list
#' 
#' 
#' 
#' 
#' 
#' 
#' 
buildAIDTable <- function(list) {
    as.data.table(list$InformationList$Information)
}

#'
#'
#'
#' 
getSynonymsFromName <- function(drugNames) {
    results <- vector(mode='list', length(drugNames))
    names(result) <- drugNames
    for (drug in drugNames) {
        results[[drug]] <-  content(getPubChem(id=drug, identifier='Name', 
            operation='synonyms'), 'parsed')
        Sys.sleep(0.6)
    }
    return(results)
}

if (sys.nframe() == 0) {
    library(httr)
    library(jsonlite)
    library(data.table)
    library(PharmacoGx)

    GDSC <- readRDS(list.files('../PSets', pattern = 'GDSC.*v2.*', full.names=TRUE))
    drugInfo <- drugInfo(GDSC)

    result <- queryPubChem(id=drugInfo$cid, identifier='cid',
    operation='aids', filter='aids_type=active')

    result <- getSynonymsFromName(drugs)

    # Get assay ids for each cid in drugInfo
    AIDtable <- buildAIDTable(result)

    # 
}