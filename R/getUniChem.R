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
    httr::RETRY(verb=verb, url=endpoint_url, ...)
}


#' Fetch a table of cheminformatic databases which can be mapped between using
#' the UniChem 2.0 API
#'
#' @param metadata `logical(1)` Should all the columns of the table be returned?
#' Default is `FALSE`, which selects only the database name ("name") and
#' UniChem source identifier ("sourceID") columns. When `TRUE` includes all
#' available source metadata, including long form descriptions of the databases,
#' their respective URLs, and much more.
#' @param ... `pairlist` Fall through parameters to `httr::GET` via `httr:RETRY`.
#' Pass `httr::verbose()` to see full details of the query being constructed.
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
getUniChemSources <- function(metadata=FALSE, ...) {
    stopifnot(is.logical(metadata))
    response <- queryUniChem(endpoint="sources", ..., verb="GET")
    response_list <- jsonlite::parse_json(response, encoding="UTF-8")
    source_dt <- data.table::rbindlist(
        response_list[[2]],
        fill=TRUE,
        use.names=TRUE
    )
    keep_cols <- if (metadata) colnames(source_dt) else c("name", "sourceID")
    return(source_dt[, keep_cols, with=FALSE])
}

## TODO:: Remove the sourceID and allow this to be specified via type!

#' Query the UniChem 2.0 compounds endpoint using POST requests
#'
#' @description
#' Retrieve database specific identifiers from all the database available in
#' UniChem based on some query compound. These identifiers can then be used
#' to reliably look up the compound in any of the included databases.
#'
#' @param type `character(1)` The kind of compound representation for the
#' molecule to search. Options are "uci" for UniChem ID, "inchi" for InChI,
#' "inchikey" for InChiKey or "sourceID" mapping between databases.
#' @param compound `character(1)` Machine readable compound identifier to the
#' specified `type`. When `type=="sourceID"` the compound must be a valid
#' identifer from the
#' @param sourceID `numeric(1)` or `character(1)` Either a UniChem source database
#' integer id or the name of a database to look up the key for. This should
#' match "sourceID" from `getUniChemCompound()` when sourceID is numeric or
#' "name" when it is character. Default source ID is "pubchem", accepting
#' valid PubChem compound IDs.
#' @param ... `pairlist` Fall through parameters to `httr::POST` via
#' `httr:RETRY`. Pass `httr::verbose()` to see full details of the query being
#' constructed.
#'
#' @return A `data.table` of database specific compound identifiers for the
#'     queried `compound`. Also attaches the query parameters ("query") and
#'     detailed InChi strutural information ("inchi") in the table attributes.
#'     See `attributes()` of the returned object for this information.
#'
#' @details
#' For cases where sourceID is character but is not in the "name" column of
#' the source database table then we try to coerce to integer under the
#' assumption that you accidentally specified the sourceID as a string.
#'
#' Full documentation:
#' https://chembl.gitbook.io/unichem/unichem-2.0/unichem-2.0-beta/api/compound-search
#'
#' @author
#' Christopher Eeles (christopher.eeles@uhnresearch.ca)
#'
#' @examples
#' \dontrun{
#'   # Look up for Erlotinib via DrugBank ID
#'   (res <- queryUniChemCompounds(compound="DB00530", type="sourceID",
#'       sourceID="drugbank"))
#'   # Now to backwards look ups with the results
#'   (erl <- queryUniChemCompounds(compound=unique(res$uci), type="uci"))
#'   (erl <- queryUniChemCompounds(compound=unique(res$inchikey), type="inchikey"))
#' }
#'
#' @export
queryUniChemCompounds <- function(compound,
        type=c("uci", "inchi", "inchikey", "sourceID"), sourceID="pubchem", ...) {
    type <- match.arg(type)
    if (type == "uci") compound <- as.character(compound)
    stopifnot(is.character(compound) && length(compound) == 1)

    if (type == "sourceID") {
        suppressWarnings({
            src_tbl <- getUniChemSources()
        })
        src_id <- sourceID
        if (!is.numeric(sourceID)) {
            src_id <- which(src_tbl$name %in% sourceID)
        }
        if (!is.numeric(src_id)) {
            src_id <- as.integer(sourceID)
        }
        stopifnot(is.numeric(src_id))
    }
    body <- c(
        list(
            type=type,
            compound=compound
        ),
        if (type == "sourceID") list(sourceID=src_id)
    )

    response <- queryUniChem(endpoint="compounds", body=body, encode="json",
        ...)
    res_list <- jsonlite::parse_json(response, encoding="UTF-8")

    # test some assumptions about the returned data
    stopifnot(length(res_list[[1]]) == 1)
    stopifnot(names(res_list[[1]][[1]]) == c("inchi", "sources", "standardInchiKey", "uci"))

    # parse into a table
    res_dt <- rbindlist(res_list[[1]][[1]][["sources"]])
    res_dt$uci <- res_list[[1]][[1]][["uci"]]
    res_dt$inchikey <- res_list[[1]][[1]][["standardInchiKey"]]
    attributes(res_dt)$inchi <- res_list[[1]][[1]][["inchi"]]
    attributes(res_dt)$query <- match.call()

    return(res_dt)
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
