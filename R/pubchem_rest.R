#' Retrieve PubChem compound information
#'
#' This function retrieves compound information from PubChem using the PubChem REST API.
#'
#' @param ids A vector of compound identifiers.
#' @param from The source namespace of the compound identifiers. Default is 'cid'.
#' @param to The target namespace for the compound information. Default is 'property'.
#' @param properties A character vector specifying the properties to retrieve.
#' @param raw Logical indicating whether to return the raw query results. Default is FALSE.
#' @param query_only Logical indicating whether to only perform the query without retrieving the results. Default is FALSE.
#' @param output The format of the query results. Default is 'JSON'.
#' @param ... Additional arguments to be passed to the query_pubchem_rest function.
#'
#' @return A data.table containing the retrieved compound information.
#'
#' @examples
#' properties=c('Title', 'MolecularFormula', 'InChIKey', 'CanonicalSMILES')
#' getPubchemCompound(c(3672, 176870), from = 'cid', to = 'property', properties = properties)
#'
#' @export
getPubchemCompound <- function(
    ids, from = 'cid', to = 'property', properties = c('Title', 'InChIKey'),
    raw = FALSE, query_only = FALSE,output = 'JSON', ...
    ){
    funContext <- .funContext("getPubchemCompound")

        
    to_ <- if(to == 'property'){
        checkmate::assert_atomic(properties, all.missing = FALSE)
        checkmate::assert_character(properties)
        to <- paste0(to, '/', paste0(properties, collapse = ','))
    }else to

    requests <- lapply(ids, function(x) {
        .build_pubchem_rest_query(
            id = x, domain = 'compound', namespace = from, operation = to_, output = output,
            raw = raw, query_only = query_only, ...)
        }
    )
    if(query_only) return(requests)

    resps_raw <- httr2::req_perform_sequential(requests, on_error = "continue")
    .debug(funContext, " Number of responses: ", length(resps_raw))
    names(resps_raw) <- ids
    if(raw) return(resps_raw)


    # Parse the responses
    resps <- .parse_pubchem_rest_responses(resps_raw)
    failed <- sapply(resps_raw, httr2::resp_is_error, USE.NAMES = T)

    if(any(failed)){
        .warn(funContext, " Some queries failed. See the 'failed' object for details.")
        failures <- lapply(resps_raw[failed], function(resp){
            .parse_resp_json(resp)$Fault
        })
    }else failures <- NULL

    if(from != 'name'){
        responses <- data.table::rbindlist(resps, fill= TRUE)
    }else{
        responses <- data.table::rbindlist(resps, idcol = from, fill = TRUE)
    }
    data.table::setnames(responses, 'V1', to, skip_absent=TRUE)

    attributes(responses)$failed <- failures 

    responses
}


#' Map compound names to PubChem CIDs
#'
#' This function maps compound names to PubChem CIDs using the PubChem REST API.
#'
#' @param names A character vector of compound names.
#' @param raw Logical indicating whether to return the raw response from the API (default is FALSE).
#' @param query_only Logical indicating whether to only perform the query without retrieving the data (default is FALSE).
#' @param output The format of the output, either 'JSON' or 'XML' (default is 'JSON').
#' @param ... Additional arguments to be passed to the getPubchemCompound function.
#'
#' @return A character vector of PubChem CIDs.
#'
#' @examples
#' mapCompound2CID(c("aspirin", "caffeine"))
#'
#' @export
mapCompound2CID <- function(
    names, raw = FALSE, query_only = FALSE, output = 'JSON', first = FALSE, ...
){
    result <- getPubchemCompound(
        ids = names, from = 'name', to = 'cids', raw = raw, query_only = query_only, output = output, ...
    )

    if(first) return(result[!duplicated(name),])
    else return(result)
}

.parse_pubchem_rest_responses <- function(responses){
    checkmate::assert_list(
        x = responses,
        any.missing = FALSE,
        names = 'named',
        min.len = 1
    )

    responses_parsed <- lapply(names(responses), function(i){
        resp <- responses[[i]]
        body <- .parse_resp_json(resp)
        if(httr2::resp_is_error(resp)) return(.parseQueryToDT(NA_integer_))

        return(.parseQueryToDT(body))
    })
    names(responses_parsed) <- names(responses)
    return(responses_parsed)
    
}





#' Build a query for the PubChem REST API
#'
#' This function builds a query for the PubChem REST API based on the provided parameters.
#'
#' @param id The identifier(s) for the query. If namespace is 'name', id must be a single value.
#' @param domain The domain of the query. Options are 'compound', 'substance', 'assay', 'cell', 'gene', 'protein'.
#' @param namespace The namespace of the query. Options depend on the chosen domain.
#' @param operation The operation to perform. Options depend on the chosen domain and namespace.
#' @param output The desired output format. Options are 'JSON', 'XML', 'SDF', 'TXT', 'CSV'.
#' @param url The base URL for the PubChem REST API.
#' @param raw Logical indicating whether to return the raw response or parse it.
#' @param query_only Logical indicating whether to return the query URL only.
#' @param ... Additional arguments to be passed to the query.
#'
#' @return The query URL or the parsed response, depending on the arguments.
#'
#' @importFrom checkmate assert assert_choice assert_logical assert_atomic test_choice assert_integerish test_atomic
#'
#' @noRd
#' @keywords internal
.build_pubchem_rest_query <- function(
        id, domain='compound', namespace='name', operation='cids',
        output='JSON', url='https://pubchem.ncbi.nlm.nih.gov/rest/pug',
        raw=FALSE, query_only=FALSE, ...){


    # -------------------------------------- Argument checking --------------------------------------
    assert_choice(domain, c('compound', 'substance', 'assay', 'cell', 'gene', 'protein'))
    switch(domain,
        "compound" = {
            assert_choice(namespace, c('cid', 'name', 'smiles', 'inchi', 'sdf', 'inchikey', 'formula'))
            assert(test_choice(
                operation, c('record', 'synonyms', 'sids', 'cids', 'aids', 'assaysummary')) ||
                    grepl('property', operation))
        },
        "substance" = assert_choice(namespace, c('sid', 'sourceid', 'sourceall', 'name')),
        "assay" = assert_choice(namespace, c('aid', 'listkey', 'type', 'sourceall', 'target', 'activity')),
        "cell" = assert_choice(namespace, c('cellacc', 'synonym')),
        "gene" = assert_choice(namespace, c('geneid', 'genesymbol', 'synonym')),
        "protein" = assert_choice(namespace, c('accession', 'gi', 'synonym'))
    )
    assert_choice(output, c('JSON', 'XML', 'SDF', 'TXT', 'CSV'))
    assert_logical(raw, query_only)
    if(!test_atomic(id, any.missing = FALSE)) .err("id must be an atomic vector with no missing/NA values")

    if(namespace == 'cid') assert_integerish(id)

    # -------------------------------------- Function context --------------------------------------
    funContext <- .funContext("query_pubchem_rest")
    if(length(id) > 1 && namespace == 'name') .err(funContext, " id must be a single value when namespace is 'name'")

    url <- .buildURL(url, domain, namespace, id, operation, output)
    .debug(funContext, " Query URL: ", url)
    if(query_only) return(url)

    # -------------------------------------- Querying PubChem REST API --------------------------------------
    .build_pubchem_request(url)
}

