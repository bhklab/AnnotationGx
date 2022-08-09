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
httpRequestUniChem <- function(endpoint, ..., verb="POST",
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
    response <- httpRequestUniChem(endpoint="sources", ..., verb="GET")
    response_list <- jsonlite::parse_json(response, encoding="UTF-8")
    source_dt <- data.table::rbindlist(
        response_list[[2]],
        fill=TRUE,
        use.names=TRUE
    )
    keep_cols <- if (metadata) colnames(source_dt) else c("name", "sourceID")
    return(source_dt[, keep_cols, with=FALSE])
}


#' Make POST requests to UniChem 2.0 compounds or connectivity endpoint
#'
#' @description
#' Retrieve database specific identifiers from all the databases available in
#' UniChem based on some query compound. These identifiers can then be used
#' to reliably look up the compound in any of the included databases.
#'
#' @param compound `character(1)` Machine readable compound identifier to the
#' specified `type`. When `type=="sourceID"` the compound must be a valid
#' identifer from the from specified `sourceID` database.
#' @param type `character(1)` The kind of compound representation for the
#' molecule to search. Options are "uci" for UniChem ID, "inchi" for InChI,
#' "inchikey" for InChIKey or "sourceID" mapping between databases. Default
#' is "sourceID".
#' @param sourceID `numeric(1)` or `character(1)` Either a UniChem source
#' database integer id or the name of a database to look up the key for.
#' This should match "sourceID" from `getUniChemCompound()` when sourceID is
#' numeric or "name" when it is character. Default source ID is "pubchem",
#' accepting valid PubChem compound IDs.
#' @param ... `pairlist` Fall through parameters to `httr::POST` via
#' `httr:RETRY`. Pass `httr::verbose()` to see full details of the query being
#' constructed.
#' @param connectivity `logical(1)` Should the connectivity API be queried
#' instead? This will treat your compound as a mixture and return sub-components,
#' isotopes or other slight variations on the query molecule.
#' Default is `FALSE`, which only matches exactly. Note that less detailed
#' structural information is returned when `connectivity=TRUE`.
#'
#' @return A `data.table` of database specific compound identifiers for the
#' queried `compound`. Also attaches the query parameters ("query") and
#' detailed InChI strutural information ("inchi") in the table attributes.
#' See `attributes()` of the returned object for this information.
#'
#' @details
#' For cases where sourceID is character but is not in the "name" column of
#' the source database table then we try to coerce to integer under the
#' assumption that you accidentally specified the sourceID as a string.
#'
#' When `type="uci"`, `compound` is automatically coerced to character.
#'
#' When `connectivity=TRUE` the connectivity end-point is queried for similar
#' compounds. The returned table in this case will have a list column which
#' indicates which criteria were met for the match.
#'
#' Full documentation:
#' https://chembl.gitbook.io/unichem/unichem-2.0/unichem-2.0-beta/api/compound-search
#' https://chembl.gitbook.io/unichem/unichem-2.0/unichem-2.0-beta/api/connectivity-search
#'
#' @author
#' Christopher Eeles (christopher.eeles@uhnresearch.ca)
#'
#' @examples
#' \donttest{
#'   # Look up for Erlotinib via DrugBank ID
#'   erl_drugbank <- postRequestUniChem(compound="DB00530", type="sourceID", sourceID="drugbank")
#'   # Now do backwards look ups with the results
#'   erl_uci <- postRequestUniChem(compound=unique(erl_drugbank$uci), type="uci")
#'   erl_ichikey <- postRequestUniChem(compound=unique(erl_drugbank$inchikey), type="inchikey")
#' }
#'
#' @export
postRequestUniChem <- function(compound,
        type=c("sourceID", "uci", "inchi", "inchikey"), sourceID="pubchem", ...,
        connectivity=FALSE) {
    # input validation
    type <- match.arg(type)
    if (type == "uci") compound <- as.character(compound)
    stopifnot(is.character(compound) && length(compound) == 1)

    # construct the API query
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
        if (type == "sourceID") list(sourceID=src_id),
        if (isTRUE(connectivity)) list(searchComponents=TRUE)
    )

    # query the API
    response <- httpRequestUniChem(
        endpoint=if (isTRUE(connectivity)) "connectivity" else "compounds",
        body=body,
        encode="json",
        ...
    )
    res_list <- jsonlite::parse_json(response, encoding="UTF-8")

    ## TODO:: Redesign function to remove this branching logic!
    if (isFALSE(connectivity)) {
        if (res_list$response == "Not found") {
            stop("Query succeeded with no matching compounds found!")
        }

        # test some assumptions about the returned data
        stopifnot(
            length(res_list[[1]]) == 1
        )
        stopifnot(
            names(res_list[[1]][[1]]) == c("inchi", "sources", "standardInchiKey", "uci")
        )

        # parse into a table
        res_dt <- rbindlist(res_list[[1]][[1]][["sources"]])
        res_dt$uci <- res_list[[1]][[1]][["uci"]]
        res_dt$inchikey <- res_list[[1]][[1]][["standardInchiKey"]]
        attributes(res_dt)$inchi <- res_list[[1]][[1]][["inchi"]]
    } else {
        for (i in seq_along(res_list[["sources"]])) {
            compare <- res_list[["sources"]][[i]][["comparison"]]
            res_list[["sources"]][[i]][["comparison"]] <-
                paste0(names(compare)[!unlist(compare)], collapse=";")
        }
        res_dt <- rbindlist(res_list[["sources"]])
        res_dt[compare == "", compare := "exact"]
        res_dt$uci <- res_list[["searchedCompound"]][["uci"]]
        res_dt$inchikey <- res_list[["searchedCompound"]][["standardInchiKey"]]
        attributes(res_dt)$inchi <- res_list[["searchedCompound"]][["inchi"]]
    }

    attributes(res_dt)$query <- match.call()
    return(res_dt)
}


#' Use the UniChem 2.0 API to map compound identifiers between different public
#' databases
#'
#' @inherit postRequestUniChem
#'
#' @param compound `character()` Vector of machine readable compound identifiers
#' for the specified `type`. When `type="sourceID"` the compounds must be valid
#' identifers from the from specified `sourceID` database. When compound is omitted
#' this function calls `getUniChemSources` and displays available databases to
#' map between.
#' @param BPPARAM A valid parallelization back-end to the
#' [`BiocParallel::bplapply`] function. Defaults to the current system
#' back-end via [`BiocParallel::bpparam()`].
#'
#' @return `data.table` Of database specific identifiers, where the `compound`
#' column contains the query compound. Rows where `compoundId` and other
#' idenfiers are NA indicate that the query for this compound failed. See
#' `attr(<result>, "failed")` to see the failure error messages.
#'
#' @examples
#' \donttest{
#'   # successful query for Erlotinib and Paclitaxel via DrugBank ID
#'   (success <- queryUniChem(compound=c("DB00530", "DB01229"), type="sourceID", sourceID="drugbank"))
#'   # partially successful query for Erlotinib and a dummy value via DrugBank ID
#'   (failed <- queryUniChem(compound=c("DB00530", "not_a_valid_id"), type="sourceID", sourceID="drugbank")
#' }
#'
#' @export
queryUniChem <- function(compound,
        type=c("sourceID", "uci", "inchi", "inchikey"), sourceID="pubchem", ...,
        connectivity=FALSE, BPPARAM=BiocParallel::bpparam()) {
    # -- early return with source options when compound is missing
    if (missing(compound)) return(getUniChemSources())

    stopifnot(is.character(compound))

    BiocParallel::bpstopOnError(BPPARAM) <- FALSE
    res <- BiocParallel::bptry(BiocParallel::bplapply(compound,
        FUN=postRequestUniChem,
        type=type, sourceID=sourceID, connectivity=connectivity,
        BPPARAM=BPPARAM, ...))
    names(res) <- compound
    failed_queries <- !BiocParallel::bpok(res)
    res_dt <- data.table::rbindlist(res[!failed_queries], idcol="compound")
    failed_dt <- data.table(compound=compound[failed_queries])
    res_dt <- rbind(res_dt, failed_dt, fill=TRUE)
    res_dt <- res_dt[compound %in% compound]  # order to match the input
    attributes(res_dt)$failed <- setNames(res[failed_queries], compound[failed_queries])
    return(res_dt)
}