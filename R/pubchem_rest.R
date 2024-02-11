
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
#' @importFrom checkmate assert assert_choice assert_logical assert_atomic test_choice
#' @export
build_pubchem_rest_query <- function(
        id, domain='compound', namespace='name', operation='cids',
        output='JSON', url='https://pubchem.ncbi.nlm.nih.gov/rest/pug',
        raw=FALSE, query_only=FALSE, ...){


    # -------------------------------------- Argument checking --------------------------------------
    assert_choice(domain, c('compound', 'substance', 'assay', 'cell', 'gene', 'protein'))
    switch(domain,
        "compound" = {
            assert_choice(namespace, c('cid', 'name', 'smiles', 'inchi', 'sdf', 'inchikey', 'formula'))
            assert(
                test_choice(
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
    assert_atomic(id, all.missing = FALSE)

    if(length(id) > 1 && namespace == 'name') stop("id must be a single value when namespace is 'name'")

    # -------------------------------------- Function context --------------------------------------
    funContext <- .funContext("query_pubchem_rest")

    url <- .buildURL(url, domain, namespace, id, operation, output)

    if(query_only) return(url)

    # -------------------------------------- Querying PubChem REST API --------------------------------------
    .build_pubchem_request(url)
}



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
    raw = FALSE, query_only = FALSE,output = 'JSON',...
    ){

    if(to == 'property'){
        checkmate::assert_atomic(properties, all.missing = FALSE)
        checkmate::assert_character(properties)
        to <- paste0(to, '/', paste0(properties, collapse = ','))
    }

    res <- lapply(ids, function(x) {
        build_pubchem_rest_query(
            id = x, domain = 'compound', namespace = from, operation = to, output = output,
            raw = raw, query_only = query_only, ...)
        }
    )

    resps_raw <- httr2::req_perform_sequential(res, on_error = "continue")
    if(raw) return(resps_raw)

    resps <- lapply(resps_raw, function(x){ .parse_resp_json(x) |> .parseQueryToDT() })

    names(resps) <- ids

    if(from != 'name'){
        responses <- data.table::rbindlist(resps)
    }else{
        responses <- data.table::rbindlist(resps, idcol = from)
    }
    data.table::setnames(responses, 'V1', to, skip_absent=TRUE)

    responses
}

.parse_resp_json <- function(resp){
    httr2::resp_body_json(resp, simplifyVector = TRUE)
}

.parseQueryToDT <- function(resp){
    data.table::as.data.table(resp[[1]][[1]])
}

