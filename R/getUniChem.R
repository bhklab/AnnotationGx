## =====================================================
## Make GET and POST Requests to the UniChem 2.0 REST API
## -----------------------------------------------------

#' @importFrom httr GET POST RETRY verbose timeout headers upload_file content
#'     http_status warn_for_status stop_for_status
#' @importFrom jsonlite fromJSON parse_json
NULL

#' Generic helper method to make arbitary REST API request to UniChem v2
#'
#' @param endpoint `character(1)` The endpoint to query. Valid endpoints at
#' time of writing are "sources", "compounds" and "connectivity".
#' Appended to the end of `url` with "/".
#' @param ... Fall through arguments to `httr::RETRY`.
#' @param url `character(1)` UniChem API URL. Don't change unless you know what
#' you are doing.
#' @param verb `character(1)` HTTP verb to query the API with. Valid options
#' for UniChem are "GET" and "POST".
#'
#' @return `httr::response` object with the result of your query.
#'
#' @export
queryUniChem <- function(endpoint, ..., verb="POST",
        url="https://www.ebi.ac.uk/unichem/api/v1") {
    stopifnot(is.character(endpoint) && length(endpoint) == 1)
    stopifnot(is.character(url) && length(url) == 1)
    stopifnot(is.character(verb) && length(verb) == 1)
    if (!endpoint %in% c("sources", "compounds", "connectivity"))
        warning("Unknown API endpoint specified: ", endpoint,
            ". Your query will likely fail!")
    switch(endpoint,
        "sources"=stopifnot(verb == "GET"),
        "compounds"=stopifnot(verb == "POST"),
        "connectivity"=stopifnot(verb == "POST")
    )
    endpoint_url <- paste0(url, "/", endpoint)
    RETRY(verb=verb, url=endpoint_url, ...)
}

#' Fetch a list of cheminformatic databases which can be mapped between using
#' the UniChem 2.0 API
#'
#' @param metadata `logical(1)` Should all the columns of the table be returned?
#' Default is `FALSE`, which selects only the database name ("name") and
#' UniChem source identifier ("sourceID") columns. When `TRUE` includes all
#' available source metadata, including long form descriptions of the databases,
#' their respective URLs, and much more.
#' @param ... `pairlist` Fall through parameters to `httr::GET` via `httr:RETRY`.
#' Pass `httr::verbose()` to see full details of the query being constructed.
#' @param endpoint `character(1)` API endpoint to query. Don't change this
#' unless you know what you are doing!
#' @param url `character(1)` URL to query. Don't change this unless you know
#' what you are doing!
#'
#' @return `data.table` Table of UniChem source databases which can be mapped
#' between. The "name" column is the database name, and the "sourceID" column
#' is its unique integer ID in UniChem. When `metadata=TRUE` also includes
#' additional source database metadata.
#'
#' @details
#' For more details see the Unichem 2.0 API documentation at:
#' https://chembl.gitbook.io/unichem/unichem-2.0/unichem-2.0-beta/whats-new
#'
#' @author
#' Christopher Eeles (christopher.eeles@uhnresearch.ca)
#'
#' @export
getUniChemSources <- function(metadata=FALSE, ...,
        endpoint="sources", url="https://www.ebi.ac.uk/unichem/api/v1") {
    stopifnot(is.logical(metadata))
    stopifnot(is.character(endpoint) && length(endpoint) == 1)
    response <- queryUniChem(endpoint=endpoint, ..., url=url, verb="GET")
    response_list <- parse_json(response)
    source_dt <- rbindlist(
        response_list[[2]],
        fill=TRUE,
        use.names=TRUE
    )
    keep_cols <- if (metadata) colnames(source_dt) else c("name", "sourceID")
    return(source_dt[, keep_cols, with=FALSE])
}


#' Query the UniChem 2.0 compounds endpoint using POST requests
#'
#' @param ... `pairlist` Fall through parameters to `httr::POST` via
#' `httr:RETRY`. Pass `httr::verbose()` to see full details of the query being
#' constructed.
#' @param url `character(1)` URL to query. Don't change this unless you know
#' what you are doing!
#'
#' @author
#' Christopher Eeles (christopher.eeles@uhnresearch.ca)
#'
#' @export
queryUniChemCompound <- function(...) {
    body <- list(

    )
    response <- queryUniChem(endpoint="compounds", body=body, ...)
}


#' Query the UniChem 2.0 connectivity endpoint using POST requests
#'
#' @param ... `pairlist` Fall through parameters to `httr::POST` via
#' `httr:RETRY`. Pass `httr::verbose()` to see full details of the query being
#' constructed.
#'
#' @author
#' Christopher Eeles (christopher.eeles@uhnresearch.ca)
#'
#' @export
queryUniChemConnectivity <- function(...) {
    body <- list(

    )
    response <- queryUniChem(endpoint="connectivity", body=body, ...)
}



if (sys.nframe() == 0) {
  # Source utility functions
  source("R/utils.R")
  source("R/parseJSON.R")

  # Load required libraries
  library(jsonlite)
  library(httr)
  library(BiocParallel)
  library(data.table)

  # Example code
  inchi <- "AAKJLRGGTJKAMG-UHFFFAOYSA-N"
  target_names <- c("chembl", "pubchem")
  type <- "key"
  ve <- c("LSXUTRRVVSPWDZ-WOTGAKFBSA-N", "AAKJLRGGTJKAMG-UHFFFAOYSA-N", "BCFGMOOMADDAQU-UHFFFAOYSA-N")
  ve2 <- c("CHEMBL12", "CHEMBL11")
  ve3 <- c("CHEMBL12", "CHEMBL11")
  # Specified target names
  database_specific_ids <- inchiToDatabaseID(inchi=inchi,
    target_names=target_names)

  # All database identifiers
  database_ids <- inchiToDatabaseID(inchi=inchi)
  test <- identifierToInchikey("CHEMBL12", "chembl")
  test2 <- wInchiToDatabaseID(ve, target_names = target_names)
  test3 <- wIdentifierToInchiKey(ve2, target_names = "chembl")
  test4 <- wMapBetweenSources(ve3, src_name="chembl", target_name="pubchem")
  test5 <- identifierToInchikey("CHEMBL12", "chembl")
  test6 <- mapBetweenSources("CHEMBL12", "chembl", "pubchem")
  #test7 <- inchiToDatabaseID("LSXUTRRVVSPWDZ-WOTGAKFBSA-N", "chembl")
}
