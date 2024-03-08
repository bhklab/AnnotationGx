#' Parses the query response into a data table
#'
#' This function takes a query response and converts it into a data table using the `as.data.table` function from the `data.table` package.
#'
#' @param resp The query response to be parsed
#' @return A data table containing the parsed query response
#' 
#' @noRd
#' @keywords internal
.parseQueryToDT <- function(resp){
    data.table::as.data.table(resp[[1]][[1]])
}


#' Parses PubChem REST responses
#'
#' This function takes a list of PubChem REST responses and parses them into a
#' standardized format. It checks the input for validity and handles error
#' responses appropriately.
#'
#' @param responses A list of PubChem REST responses.
#' @return A list of parsed PubChem responses, with each response parsed into a
#'         data table format.
#'
#' @noRd
#' @keywords internal
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

