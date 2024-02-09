.buildURL <- function(...) paste0(na.omit(unlist(list(...))), collapse='/')

query_pubchem_rest <- function(
        id, domain='compound', namespace='name', operation='cids',
        output='JSON', url='https://pubchem.ncbi.nlm.nih.gov/rest/pug',
        raw=FALSE, query_only=FALSE, verbose = FALSE,...){


    # -------------------------------------- Argument checking --------------------------------------
    checkmate::assert_choice(domain, c('compound', 'substance', 'assay', 'cell', 'gene', 'protein'))
    switch(domain,
        compound = {
            checkmate::assert_choice(namespace, c('cid', 'name', 'smiles', 'inchi', 'sdf', 'inchikey', 'formula'))
            checkmate::assert(
                checkmate::test_choice(operation, c('record', 'synonyms', 'sids', 'cids', 'aids', 'assaysummary')) ||
                # For property queries it must start with property so return TRUE if contains
                grepl('property', operation))
        },
        substance = checkmate::assert_choice(namespace, c('sid', 'sourceid', 'sourceall', 'name')),
        assay = checkmate::assert_choice(namespace, c('aid', 'listkey', 'type', 'sourceall', 'target', 'activity')),
        cell = checkmate::assert_choice(namespace, c('cellacc', 'synonym')),
        gene = checkmate::assert_choice(namespace, c('geneid', 'genesymbol', 'synonym')),
        protein = checkmate::assert_choice(namespace, c('accession', 'gi', 'synonym'))
    )
    checkmate::assert_choice(output, c('JSON', 'XML', 'SDF', 'TXT', 'CSV'))
    checkmate::assert_logical(raw, query_only, verbose)

    checkmate::assert_atomic(id, all.missing = FALSE)
    if(length(id) > 1 && namespace == 'name') stop("id must be a single value when namespace is  'name'")


    # -------------------------------------- Function context --------------------------------------
    funContext <- .funContext("query_pubchem_rest")

    url <- .buildURL(url, domain, namespace, id, operation, output)
    url <- URLencode(url)
    if(verbose) .debug(funContext, "URL: ", url)
    if(query_only) return(url)


    # -------------------------------------- Querying PubChem REST API --------------------------------------
    req <- httr2::request(url)

    resp <- req |>
        httr2::req_retry(max_tries = 3) |> httr2::req_throttle(rate = 400/60) |> req_error(is_error = \(resp) FALSE) |> httr2::req_perform()

    if(output != 'JSON') .err(funContext, "Only JSON output is supported")
    resp <- resp |>  httr2::resp_body_json()
    # checkmate::assert_class(resp, "httr2_response")
    return(resp)
}


getPubchemCompound <- function(
    ids, from = 'cid', to = 'property', properties = c('Title', 'InChIKey'),
    batch = TRUE, raw = FALSE, query_only = FALSE, verbose = FALSE, output = 'JSON',
    BPPARAM = BiocParallel::SerialParam(),...
    ){

    if(from != 'cid') {
        .warn("Batch Queries cannot be used when mapping from name. Setting to False")
        batch <- FALSE
    }

    if(to == 'property'){
        checkmate::assert_atomic(properties, all.missing = FALSE)
        checkmate::assert_character(properties)

        to <- paste0(to, '/', paste0(properties, collapse = ','))
    }
    res <- BiocParallel::bplapply(ids, function(x) {
        query_pubchem_rest(
            id = x, domain = 'compound', namespace = from, operation = to, output = output,
            raw = raw, query_only = query_only, verbose = verbose, ...
        )
    },
    BPPARAM = BPPARAM
    )
    names(res) <- ids
    if(verbose) .info(res)
    .parseQueryToDT <- function(queryRes){
        data.table::as.data.table(queryRes[[1]][[1]]) |> data.table::transpose()
    }

    queryRes <- BiocParallel::bplapply(res, FUN=.parseQueryToDT)
    if(verbose) .info(print(queryRes))

    queryRes <- rbindlist(queryRes, idcol=from)
    data.table::setnames(queryRes, 'V1', to, skip_absent=TRUE)
    queryRes
}



