#' Constructs and executes a GET request to the PubChem REST API
#'
#' @description
#' This function builds a query to the PubChem REST API based on the function
#' parameters then executes that query.
#'
#' @details
#' See https://pubchemdocs.ncbi.nlm.nih.gov/pug-rest for the complete API 
#' documentation.
#'
#' 
#'
#' @param id
#' @param input
#' @param identifier
#' @param operation
#' @param output
#' @param ... Force subsequent arguments to be named. Not used.
#' @param url The URL of the PubChem REST API.
#' @param fitler 
#'
#' @return A `response` object
#'
#' @seealso [httr::GET], []
#'
#' @md
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
    encodedQuery <- URLencode(query, reserved=TRUE)
    print(encodedQuery)

    # get HTTP response
    GET(encodedQuery)
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
#'   JSON to a list. This only works when `output='JSON'` in `getPubChem`.
#' 
#' @param ... Fallthrough arguments to `AnnotationGx::getPubChem` function.
#' 
#' @md
#' @export
queryPubChem <- function(...) parseJSON(getPubChem(...))


## ============================
## queryPubChem wrapper methods
## ----------------------------


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
querySynonymsFromName <- function(drugNames) {
    ## TODO:: Design a general mechanism for correcting special character
    safeDrugNames <- drugNames
    results <- vector(mode='list', length(drugNames))
    names(results) <- safeDrugNames
    # Quote all drug names for safety
    for (drug in safeDrugNames) {
        results[[drug]] <- content(
            getPubChem(
                id=drugs[5], 
                identifier='Name', 
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

    result <- querySynonymsFromName(drugs)
    unlist_result <- lapply(result, unlist)
    expSynDT <- data.table(drugid=drugs, synonyms=unlist_result)
    expSynDT[, synonyms2 := mapply(c, drugid, synonyms)]
    mappedSynonyms <- expSynDT[, .(list(which(..lab_drugids %in% unlist(.SD$synonyms2)))), by=drugid]
    mappedSynonyms <- mappedSynonyms[, unlist(V1), by=drugid]

    unmapped <- setdiff(expSynDT$drugid, mappedSynonyms$drugid)
    unmappedDT <- expSynDT[drugid %in% unmapped]

    # Get assay ids for each cid in drugInfo
    AIDtable <- buildAIDTable(result)

    # 
}