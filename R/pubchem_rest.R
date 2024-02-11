.buildURL <- function(...) {
    paste0(stats::na.omit(unlist(list(...))), collapse='/') |> utils::URLencode()
}


.build_pubchem_request <- function(url){
    httr2::request(url) |>
        httr2::req_retry(max_tries = 3) |>
        httr2::req_throttle(rate = 400/60) |>
        httr2::req_error(is_error = \(resp) FALSE)
}


# .send_pubchem_request <- function(url, query_only = FALSE, verbose = FALSE, ...){
#     resp <- req |>
#         httr2::req_retry(max_tries = 3) |>
#         httr2::req_throttle(rate = 400/60) |>
#         httr2::req_error(is_error = \(resp) FALSE) |>
#         httr2::req_perform()

#     return(resp)
# }


#' Query PubChem REST API
#'
#' This function queries the PubChem REST API to retrieve information
#' about compounds, substances, assays, cells, genes, and proteins.
#'
#' @param id The identifier(s) of the entity to query. For compound queries, it can be a single value or a vector of values. For other domains, it must be a single value.
#' @param domain The domain to query. Options are 'compound', 'substance', 'assay', 'cell', 'gene', and 'protein'.
#' @param namespace The namespace to use for the query. The available options depend on the chosen domain.
#' @param operation The operation to perform. The available options depend on the chosen domain and namespace.
#' @param output The desired output format. Options are 'JSON', 'XML', 'SDF', 'TXT', and 'CSV'.
#' @param url The base URL of the PubChem REST API.
#' @param raw Logical indicating whether to return the raw response or parse it into a structured format.
#' @param query_only Logical indicating whether to return the constructed query URL without making the actual request.
#' @param verbose Logical indicating whether to print debug information.
#' @param ... Additional arguments to be passed to the underlying HTTP request function.
#'
#' @return The response from the PubChem REST API, parsed into a structured format if raw is set to FALSE.
#'
#' @references
#' PubChem REST API documentation: https://pubchemdocs.ncbi.nlm.nih.gov/pug-rest
#'
#' @importFrom checkmate assert_choice assert assert_logical assert_atomic test_choice
#' @export
build_pubchem_rest_query <- function(
        id, domain='compound', namespace='name', operation='cids',
        output='JSON', url='https://pubchem.ncbi.nlm.nih.gov/rest/pug',
        raw=FALSE, query_only=FALSE, verbose = FALSE,  ...){


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
    assert_logical(raw, query_only, verbose)
    assert_atomic(id, all.missing = FALSE)

    if(length(id) > 1 && namespace == 'name') stop("id must be a single value when namespace is 'name'")

    # -------------------------------------- Function context --------------------------------------
    funContext <- .funContext("query_pubchem_rest")

    url <- .buildURL(url, domain, namespace, id, operation, output)
    if(verbose) .debug(funContext, "URL: ", url)
    if(query_only) return(url)

    # -------------------------------------- Querying PubChem REST API --------------------------------------
    .build_pubchem_request(url)
    # if(raw) return(resp)
    # if(output != 'JSON') .err(funContext, "Only JSON output is supported")
    # resp <- resp |>  httr2::resp_body_json() |> .parseQueryToDT()
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
#' @param verbose Logical indicating whether to display verbose output. Default is FALSE.
#' @param output The format of the query results. Default is 'JSON'.
#' @param ... Additional arguments to be passed to the query_pubchem_rest function.
#'
#' @return A data.table containing the retrieved compound information.
#'
#'
#'
#' @export
getPubchemCompound <- function(
    ids, from = 'cid', to = 'property', properties = c('Title', 'InChIKey'),
    raw = FALSE, query_only = FALSE, verbose = FALSE, output = 'JSON',...
    ){

    if(to == 'property'){
        checkmate::assert_atomic(properties, all.missing = FALSE)
        checkmate::assert_character(properties)
        to <- paste0(to, '/', paste0(properties, collapse = ','))
    }

    res <- lapply(ids, function(x) {
        build_pubchem_rest_query(
            id = x, domain = 'compound', namespace = from, operation = to, output = output,
            raw = raw, query_only = query_only, verbose = verbose, ...)
        }
    )

    resps_raw <- httr2::req_perform_sequential(res, on_error = "continue")
    if(raw) return(resps_raw)

    resps <- lapply(resps_raw, function(x){
        x |>   .parse_resp_json() |> .parseQueryToDT()
        }
    )
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
