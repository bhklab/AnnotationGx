
## =========================================
## Make GET Requests to the PubChem REST API
## -----------------------------------------


#' Constructs and executes a GET request to the PubChem PUG REST API
#'
#' @description
#' This function builds a query URL for the PubChem PUG REST API based on the
#' function parameters then executes that query, returning a `httr::request`
#' object.
#'
#' @details See the PUG_VIEW website for the complete 
#'   API documentation. A subset of this documentation is included below for
#'   convenience.
#'#'
#' Most – if not all – of the information PubChem PUG service needs to
#'   produce its results is encoded into the URL. The general form of the URL
#'   has three parts – input, operation, and output – after the common prefix,
#'   followed by operation options as URL arguments (after the ‘?’):
#'
#' "https://pubchem.ncbi.nlm.nih.gov/rest/pug/<input specification>/<operation specification>/\[<output specification>\]\[?<operation_options>\]"
#'
#' ### Input
#'
#' The input portion of the URL tells the service which records to use as the
#'   subject of the query. This is further subdivided into two or more locations
#'   in the URL “path” as follows:
#'
#' "<input specification> = <domain>/<namespace>/<identifiers>"
#'
#' "<domain> = substance | compound | assay | <other inputs>"
#'
#' "compound domain <namespace> = cid | name | smiles | inchi | sdf | inchikey | formula | <structure search> | <xref> | listkey | <fast search>"
#'
#' "substance domain <namespace> = sid | sourceid/<source id> | sourceall/<source name> | name | <xref> | listkey"
#'
#' "assay domain <namespace> = aid | listkey | type/<assay type> | sourceall/<source name> | target/<assay target> | activity/<activity column name>"
#'
#' Complete documentation for valid input specifications can be found at:
#' https://pubchemdocs.ncbi.nlm.nih.gov/pug-rest$_Toc494865556
#'
#' ### Operation
#'
#' The operation part of the URL tells the service what to do with the input
#'   records – such as to retrieve whole record data blobs or specific
#'   properties of a compound, etc. The construction of this part of the
#'   “path” will depend on what the operation is. Currently, if no operation is
#'   specified at all, the default is to retrieve the entire record. What
#'   operations are available are, of course, dependent on the input domain –
#'   that is, certain operations are applicable only to compounds and not
#'   assays.
#'
#' "compound domain <operation specification> = record | <compound property> | synonyms | sids | cids | aids | assaysummary | classification | <xrefs> | description | conformers"
#'
#' "substance domain <operation specification> = record | synonyms | sids | cids | aids | assaysummary | classification | <xrefs> | description"
#'
#' "assay domain <operation specification> = record | concise | aids | sids | cids | description | targets/<target type> | <doseresponse> | summary | classification"
#'
#' Complete documentation for valid operations can be found at:
#' https://pubchemdocs.ncbi.nlm.nih.gov/pug-rest$_Toc494865557
#'
#' ### Output
#'
#' The final portion of the URL tells the service what output format is desired.
#'   Note that this is formally optional, as output format can also be specified
#'   in the HTTP Accept field of the request header – see below for more detail.
#'
#' "<output specification> = XML | ASNT | ASNB | JSON | JSONP [ ?callback=<callback name> ] | SDF | CSV | PNG | TXT"
#'
#' Complete documentation for valid output formats can be found at:
#' https://pubchemdocs.ncbi.nlm.nih.gov/pug-rest$_Toc494865558
#'
#' @param id The identifier for REST API query. This should be a valid compound
#'   identifier for the specified `domain` and `namespace`. For example, if
#'   `domain='compound'` and `namespace='cid'` then `id` should be one or
#'   more compound cids to retrieve information about. If `id` is a `list` or
#'   `vector`, the ids are parsed to a string with ids separated by
#'   commas. This parameter forms the '<identifiers>' part of the Input
#'   portion of the URL path: '<domain>/<namespace>/<identifiers>'.
#' @param domain Which records in PubChem are the subject of the query? Default
#'   is 'compound'. Usually, this will be one of 'substance', 'compound', or
#'   'assay'. For more advanced options see the PubChem PUG REST API
#'   documentation. This forms the '<domain>' part of the Input portion of the
#'   URL path.
#' @param namespace What kind of identifiers are in `id`? These options
#'   are specific to the selected domain. For the 'compound' domain, common
#'   options are 'cid', 'name', 'smiles', 'inchi', or 'formula'. For the
#'   'substance' domain common options are 'sid' or 'name'. For the
#'   'assay' domain common options are 'aid' and 'target/<assay_target>' where
#'   '<assay_target>' is one of 'gi', 'proteinname', 'geneid', 'genesymbol' or
#'   'accession'. For all options please see the PubChem PUG REST API
#'   documentation. This parameter makes up the '<namespace>' part of the
#'   Input portion of the URL path.
#' @param operation What kind of data to return for the specified input
#'   portion of the URL path? If excluded this defaults to return the entire
#'   record for that specified input. Options are domain specific and details
#'   are available in the PubChem PUG REST API documentation. Options valid for
#'   'compound', 'substance' and 'assay' domains are 'record', 'aids', 'sids',
#'   'aids', 'description' and 'classification'. Common options for the
#'   'compound' domain are 'assaysummary', 'conformers' and
#'   'property/<properties>' where '<properties>' is a comma separated list
#'   property tags; valid tags can be found at
#'   https://pubchemdocs.ncbi.nlm.nih.gov/pug-rest$_Toc494865565.
#'   Common options for the 'substance' domain are 'assaysummary'.
#'   Common options for the 'assay' domain include 'concise', 'summary' and
#'   'targets/<target type>' where '<target type>' is one of 'ProteinGI',
#'   'ProteinName', 'GeneID' or 'GeneSymbol'.
#' @param output What format should the data be returned in? Default is 'JSON'.
#'   other common options include 'XML', 'CSV' and 'TXT'. For a complete list of
#'   output format options, plase see the PubChem PUG REST API documentation.
#' @param ... Fall through arguments to [`httr::GET`].
#' @param url The URL of the PubChem REST API. Probably don't change this.
#' @param operation_options Further optional arguments for the selected operation.
#'   this is specific to the selected operation. This is appended as a string
#'   after '?' at the end of the query. See
#'   https://pubchemdocs.ncbi.nlm.nih.gov/pug-rest$_Toc494865565 for details.
#' @param query_only `logical(1)` Should this function early return only
#'   the encoded query?
#'
#' @return A `httr::response` object with the results of the GET request.
#'
#' @seealso [httr::GET], [httr::RETRY], [queryRequestPubChem]
#'
#' @references
#' Kim S, Thiessen PA, Cheng T, Yu B, Bolton EE. An update on PUG-REST: RESTful interface for programmatic access to PubChem. Nucleic Acids Res. 2018 July 2; 46(W1):W563-570. doi:10.1093/nar/gky294.
#'
#' Kim S, Thiessen PA, Bolton EE, Bryant SH. PUG-SOAP and PUG-REST: web services for programmatic access to chemical information in PubChem. Nucleic Acids Res. 2015 Jul 1; 43(W1):W605-W611. doi: 10.1093/nar/gkv396.
#'
#' Kim S, Thiessen PA, Bolton EE. Programmatic Retrieval of Small Molecule Information from PubChem Using PUG-REST. In Kutchukian PS, ed. Chemical Biology Informatics and Modeling. Methods in Pharmacology and Toxicology. New York, NY: Humana Press, 2018, pp. 1-24. doi:10.1007/7653_2018_30.
#'
#' @md
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom httr RETRY GET timeout 
#' @importFrom data.table data.table fread
#' @export
getRequestPubChem <- function(id, domain='compound', namespace='cid', operation=NA,
        output='JSON', ..., url='https://pubchem.ncbi.nlm.nih.gov/rest/pug',
        operation_options=NA, raw=FALSE, query_only=FALSE, verbose = FALSE) {
    funContext <- .funContext('::getRequestPubChem')

    # handle list or vector inputs for id
    if (all(is.na(id))) .error(funContext, 'All ids are NA!')
    if (length(id) > 1) id <- paste0(na.omit(id), collapse=',')

    # replace special characters in id
    id <- URLencode(id, reserved=TRUE, repeated=TRUE)

    # build query URL
    query <- .buildURL(url, domain, namespace, id, operation, output)
    if (!is.na(operation_options))
        query <- paste(query, operation_options, sep='?')
    encodedQuery <- URLencode(query)

    if (isTRUE(query_only)) return(encodedQuery)

    # get HTTP response, respecting the 30s max query time of PubChem API
    tryCatch({
        
        # result <- RETRY('GET', encodedQuery, timeout(29), times=9,
        #     quiet=TRUE, terminate_on=c(400, 404, 503))

        result <- RETRY('GET', encodedQuery, timeout(29), times=9,
            quiet=TRUE)

        if (isTRUE(raw)) return(result)

        .checkThrottlingStatus(result, throttleMessage = verbose)

        canParse <- tryCatch({ parseJSON(result, as='text'); TRUE },
            error=function(e) FALSE)
        if (output == 'JSON' && !canParse) stop('Parsing to JSON failed') else
            result
        },
        warning=function(w) { cat('\r'); print(w) },
        error=function(e) {
            cat('\r')
            print(e)
            # return a response JSON with the error if the query fails
            if (output == 'JSON') {
                httr:::response(
                    header=headers(result),
                    url=encodedQuery,
                    content=toJSON(list(
                        Error=list(
                            Code='getRequestPubChem.ERROR',
                            Message='See Details for error message',
                            Details=e))),
                    status_code=400)
            }
        })
}


#' @title queryPubChem
#'
#' @details
#' This function automatically parses the results of the 
#'
#' @inheritParams getRequestPubChem
#' @param ... Fall through parameters to `bpmapply`.
#'
#' @seealso [`getRequestPubChem`]
#'
#' @md
#' @importFrom BiocParallel bplapply bpparam bpnworkers bpworkers<-
#'   bpprogressbar<- bplog<-
#' @importFrom jsonlite toJSON
#' @export
queryPubChem <- function(id, domain='compound', namespace='cid', operation=NA,
        output='JSON', ..., url='https://pubchem.ncbi.nlm.nih.gov/rest/pug',
        operation_options=NA, batch=TRUE, raw=FALSE, 
        query_only=FALSE, verbose = FALSE) {
    if (!is.character(id)) id <- as.character(id)
    if (namespace %in% c('name', 'xref', 'smiles', 'inchi', 'sdf'))
        batch <- FALSE

    # Cap parallelization at 5 cores to prevent excessive requests
    BPPARAM <- list(...)[['BPPARAM']]
    if (is.null(BPPARAM)) {
        BPPARAM <- bpparam()
        if (class(BPPARAM) %in% c('MulticoreParam', 'SnowParam')) {
            if (bpnworkers(BPPARAM) > 5)
                bpworkers(BPPARAM) <- 5
        }
        bpprogressbar(BPPARAM) <- TRUE
    }

    # -- make queries
    # TODO:: Throw errors in getPubChem for bad HTTP request to allow use
    ##>of BPREDO feature
    if (batch) {
        # determine how many queries to make, with a max 4000 characters per query
        # add 2 characters for conversion of ',' as '%2C' after URL encoding
        maxNChars <- max(vapply(id, FUN=nchar, numeric(1)), na.rm=TRUE) + 2
        totalLength <- length(id) * maxNChars
        numQueries <- ceiling(totalLength / 4000)
        querySize <- ceiling(length(id) / numQueries)
        queries <- split(id, ceiling(seq_along(id) / querySize))


        queryRes <- bplapply(queries, FUN=.queryPubChemSleep, domain=domain,
            namespace=namespace, operation=operation, output=output, url=url,
            operation_options=operation_options, BPPARAM=BPPARAM,
            raw=raw, query_only=query_only, verbose = verbose)
    } else {

        queryRes <- bplapply(id, FUN=.queryPubChemSleep, domain=domain,
            namespace=namespace, operation=operation, output=output, url=url,
            operation_options=operation_options, BPPARAM=BPPARAM,
            raw=raw, query_only=query_only, verbose = verbose)
        queries <- as.list(id)
    }

    # -- early return option
    if (raw) return(queryRes)

    # -- deal with failed queries
    failed <- unlist(lapply(queryRes, names)) %in% c("Fault", "Bad", "Error")
    if (any(failed)) {
        failedQueries <- Map(list, query=queries[failed], failure=queryRes[failed])
        queryRes <- queryRes[!failed]
        queries <- queries[!failed]
        attributes(queryRes)$failed <- failedQueries
        if (is.null(queries)) queries <- NA
    }

    # -- Attach query metadata to the returned list
    attributes(queryRes)$queries <- queries

    return(queryRes)
}

#' Query the PubChem REST API, with the result automatically converted from
#'   JSON to a list. This only works when `output='JSON'` in `getRequestPubChem`.
#'
#' @param ... Fallthrough arguments to `AnnotationGx::getRequestPubChem` function.
#' @param query_only
#'
#' @md
#' @export
queryRequestPubChem <- function(..., query_only=FALSE)
    parseJSON(getRequestPubChem(..., query_only=query_only), query_only=query_only)

## TODO:: Retrieve PubChem server status to dynamically set query spacing
##>based on server load
## TODO:: Make the query away for server load status in response header
.queryPubChemSleep <- function(x, ..., query_only=FALSE) {
    queryRes <- tryCatch({
        queryRequestPubChem(x, ..., query_only=query_only)
    },
        error=function(e) {
            cat('\r')
            print(e)
        list(Error=list(
            Code='.queryPubChemSleep.ERROR',
            Message='See Details for error msg',
            Details=e
        ))
    })

    return(queryRes)
}
## ============================
## queryPubChem wrapper methods
## ----------------------------

#' @title getPubChemCompound
#'
#' @description
#' Make queries to the PubChem Compound domain.
#'
#' @param ids A `character` or `numeric` vector of valid PubChem identifiers
#'   to use for the query. Which identifier is being used must be specified in
#'   the `from` parameter.
#' @param from A `character(1)` vector with the desired namespace to query.
#'   Default is 'cid'. Try using 'sid' if some of your CIDs fail to map.
#' @param to A `character(1)` vector with the desired return type. Defaults
#'   to 'record', which returns all available data from the specified IDs.
#' @param ... Fallthrough arguments to `BiocParallel::bmapply`.
#' @param properties A `character` vector of properties to return. Only used
#'   when `to='property'`. Common properties of interest are: 'Title' (name),
#'   'IUPACName', 'CanonicalSMILES', 'IsomericSMILES', 'InChIKey'. The default
#'   setting will return 'Title'.
#'   See details for more information.
#' @param batch `logical(1)` Should the query be run in batches (i.e., multiple
#'   ids per GET request to the API). Default is `TRUE`. Should be set to
#'   `FALSE` when retrying failed queries. Batch queries are not supported when
#'   `from` is one of 'name', 'xref', 'smiles', 'inchi' or 'sdf'. In these
#'   cases, batch will automatically be set to `FALSE` with a warning.
#' @param raw `logical(1)` Should the raw query results be early returned. This
#'   can be useful for diagnosing issues with failing queries.
#'   this can increase query length, but it useful if you have been blacklisted.
#'
#' @return A `data.table` containing results of the query, or a list if `raw`
#'   is set to `TRUE`. Failed queries are available as an attribute of the
#'   returned object, see `attributes(object)`.
#'
#' @details
#' ## `properties`
#' For a full list of availabe properties see:
#' https://pubchemdocs.ncbi.nlm.nih.gov/pug-rest$_Toc494865556
#'
#' @md
#' @importFrom data.table setnames as.data.table rbindlist
#' @export
getPubChemCompound <- function(ids, from='cid', to='property', ...,
        properties='Title', batch=TRUE, raw=FALSE, options=NA,
        query_only=FALSE) {
    if (!is.character(ids)) ids <- as.character(ids)
    if (from %in% c('name', 'xref', 'smiles', 'inchi', 'sdf', 'inchikey')) {
        if (isTRUE(batch)) .warning('Batch queries cannot be used when mapping
            from name, xref, smiles, inchi, sdf, or inchikey. Setting to FALSE.')
        batch <- FALSE
    }
    if (grepl('fast', from)) {
        if (isTRUE(batch)) .warning('The fastsearch API does not support batch
            queries. Setting to FALSE.')
        batch <- FALSE
    }

    if (to == 'property') to <- paste0(to, '/', paste0(properties, collapse=','))
    
    queryRes <- queryPubChem(ids, domain='compound', namespace=from,
        operation=to, batch=batch, raw=raw, 
        operation_options=options, query_only=query_only, ...)

    # -- early return option
    if (isTRUE(raw) || isTRUE(query_only)) return(queryRes)

    # -- deal with failed queries
    queries <- attributes(queryRes)$queries
    failedQueries <- attributes(queryRes)$failed
    for (i in seq_along(queries)) {
        queryRes[[i]][[from]] <- queries
    }

    # -- process the results
    .replace_NULL_NA <- function(DT) lapply(DT, function(x) {
        ifelse(is.null(x), rep(NA_integer_, length(x)), x) })

    # TODO:: Determine if all results are wrapped in two lists? If not this may
    #>break the function.
    .parseQueryToDT <- function(queryRes) as.data.table(queryRes[[1]][[1]])
    queryRes <- lapply(queryRes, FUN=.parseQueryToDT)
    if (isFALSE(batch)) {
        names(queryRes) <- queries
        queryRes <- rbindlist(queryRes, idcol=from)
    } else {
        queryRes <- rbindlist(queryRes)
    }

    setnames(queryRes, 'V1', to, skip_absent=TRUE)
    if (from == 'sid') setnames(queryRes, 'CID', 'SID', skip_absent=TRUE)
    if (length(failedQueries) > 0) attributes(queryRes)$failed <- failedQueries

    return(queryRes)
}

#' Retrieve PubChem substance information
#'
#' This function retrieves PubChem substance information based on the provided IDs.
#' It supports mapping from various namespaces such as CID, name, xref, smiles, inchi, and sdf.
#' The function can handle batch queries and can return the raw query results if specified.
#'
#' @param ids A character vector of IDs to query.
#' @param from The namespace of the input IDs (default is 'cid').
#' @param to The namespace to map the input IDs to (default is 'sids').
#' @param batch Logical indicating whether to use batch queries (default is TRUE).
#' @param raw Logical indicating whether to return the raw query results (default is FALSE).
#' @param ... Additional arguments to be passed to the queryPubChem function.
#'
#' @return A data table containing the retrieved PubChem substance information.
#' If raw is TRUE, the function returns the raw query results.
#' If any queries fail, the function includes the failed queries in the returned object.
#'
#' @examples
#' getPubChemSubstance(c('123', '456'), from='cid', to='sids')
#' getPubChemSubstance(c('water', 'ethanol'), from='name', to='cid', batch=FALSE)
#' getPubChemSubstance(c('C1=CC=CC=C1', 'CCO'), from='smiles', to='name', raw=TRUE)
#' 
#' @references
#' Documentation for the PubChem API: https://pubchemdocs.ncbi.nlm.nih.gov/
#'
#' @export
getPubChemSubstance <- function(ids, from='cid', to='sids', ...,
        batch=TRUE, raw=FALSE) {
    if (!is.character(ids)) ids <- as.character(ids)
    if (from %in% c('name', 'xref', 'smiles', 'inchi', 'sdf')) {
        if (isTRUE(batch)) .warning('Batch queries cannot be used when mapping
            from name, xref, smiles, inchi or sdf. Setting to batch=FALSE.')
        batch <- FALSE
    }
    queryRes <- queryPubChem(ids, domain='substance',
        namespace=from, operation=to, batch=batch, raw=raw, ...)

    # -- early return option
    if (raw) return(queryRes)

    # -- deal with failed queries
    queries <- attributes(queryRes)$queries
    failedQueries <- attributes(queryRes)$failed

    # -- process the results
    .replace_NULL_NA <- function(DT) lapply(DT, function(x) {
        ifelse(is.null(x), rep(NA_integer_, length(x)), x) })

    # TODO:: Determine if all results are wrapped in two lists? If not this may
    #>break the function.
    .parseQueryToDT <- function(queryRes) as.data.table(queryRes[[1]][[1]])
    queryRes <- lapply(queryRes, FUN=.parseQueryToDT)
    if (isFALSE(batch)) {
        names(queryRes) <- queries
        queryRes <- rbindlist(queryRes, idcol=from)
    } else {
        queryRes <- rbindlist(queryRes)
    }

    setnames(queryRes, 'V1', to, skip_absent=TRUE)
    if (from == 'sid') setnames(queryRes, 'CID', 'SID', skip_absent=TRUE)
    if (length(failedQueries) > 0) attributes(queryRes)$failed <- failedQueries

    return(queryRes)
}



#' Get a selected annotation for all PubChem entries
#'
#' @description
#' Queries the PubChem PUG VIEW API to get all annotations for the specified
#'   header. Results will be mapped to CID and/or SID.
#'
#' @param header `character(1)` A valid header name for the PUG VIEW annotations
#'   API. Default is 'Available', which will return a list of available
#'   headers as a `data.frame`.
#' @param type `character(1)` The header type. Default is 'Compound'. Make
#'   sure ot change this if your header of interest isn't type compouns.
#' @param parseFUN `character(1)` or `function` A custom function to parse
#'   the results returned from this function for unkown header arguments.
#'   Defaults to identity, i.e., it returned the results unparsed. Some
#'   default parsing is implemented inside the function for 'ATC Code' and
#'   'Drug Induced Liver Injury' headers.
#' @param ... Force subsequent parameters to be named. Not used.
#' @param output `character(1)` The output format. Defaults to 'JSON'. For
#'   options other than 'JSON', you must set `raw=TRUE` or the fuction will
#'   fail.
#' @param url `character(1)` The URL to perform API queries on. This is for
#'   developer use only and should not be changed.
#' @param BPPARAM `BiocParallelParam` A BiocParallel back-end to parallelize
#'   with. Defaults to `bpparam()`. To run in serial, set to `SerialParam()`.
#'
#' @return A `data.table` of resulting annotations. If the header is not
#'   one of those mentioned in `parseFUN` documentation, then it will returned
#'   an unparsed `data.table` which will need to be futher processed to get
#'   the data interest.
#'
#' @details
#' # API Documentation
#' For detailed documentation of the annotations API see:
#' https://pubchemdocs.ncbi.nlm.nih.gov/Dpug-view$_Toc495044630
#'
#' @importFrom jsonlite fromJSON
#' @importFrom data.table data.table as.data.table merge.data.table last rbindlist fwrite
#' @importFrom BiocParallel bpparam bpworkers bpprogressbar bptry
#' @export
getAllPubChemAnnotations <- function(
    header='Available', 
    type='Compound',
    parseFUN=identity, 
    output='JSON', 
    raw=FALSE, 
    rawAnnotationDT=FALSE, 
    verbose = FALSE,
    url='https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/annotations/heading',
    BPPARAM=bpparam(),
    retries=3,
    maxPages=NA,
    ...
    ) {
    funContext <- .funContext('::getPubChemAnnotations')
    if (header == 'Available') {
        queryURL <-
            'https://pubchem.ncbi.nlm.nih.gov/rest/pug/annotations/headings/JSON'
    } else if (header == 'data') {
        url <- 'https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/'
        queryURL <- paste0(.buildURL(url, header, output),
            '?heading_type=', type)
    } 
    else {
        queryURL <- paste0(.buildURL(url, header, output),
            '?heading_type=', type)
    }
    encodedQueryURL <- URLencode(queryURL)

    
    queryRes <- RETRY('GET', encodedQueryURL, timeout(29), times=retries,
        quiet=TRUE)

    .checkThrottlingStatus(queryRes)
    if (isTRUE(raw)) return(queryRes)

    resultDT <- as.data.table(parseJSON(queryRes)[[1]][[1]])

    if (header == 'Available') return(resultDT)

    numPages <- content(queryRes)[[1]]$TotalPages
    if (is.null(numPages)) {
        numPages <- 1 
    }else {
        if (!is.na(maxPages)) numPages <- min(as.numeric(maxPages),as.numeric(numPages))
        if (verbose) print(paste0("numPages: ", numPages))
    }

    
    if (numPages > 1) {
        tryCatch({
            # bpworkers(BPPARAM) <- 5
            bpprogressbar(BPPARAM) <- TRUE
            # print(BPPARAM)
            BiocParallel::register(BPPARAM)
        }, error=function(e) .warning(funContext, 'Failed to set parallelzation
            parameters! Please configure them yourself and pass in as the
            BPPARAM argument.'))
        pageList <- bplapply(seq(2, numPages), function(i, queryURL, numPages) {
            encodedURL <- URLencode(paste0(queryURL, '&page=', i))
            queryRes <- RETRY('GET', encodedQueryURL, timeout(1), times=10,
                    quiet=TRUE)
            
            .checkThrottlingStatus(queryRes)
            
            page <- tryCatch({
                as.data.table(parseJSON(queryRes)[[1]][[1]])
            }, error=function(e) {
                .warning(funContext, 'Parsing to JSON failed! Returning empty
                    data.table.')
                return(data.table())
            })
            # if (queryTime < 0.31) Sys.sleep(0.31 - queryTime)
            return(page)
        },  queryURL=queryURL, numPages=numPages)
        pageList <- c(list(resultDT), pageList)
    } else {
        pageList <- list(resultDT)
    }

    if (header != 'CAS') {
        annotationDT <- rbindlist(pageList, fill=TRUE, use.names=TRUE)
        if (verbose) print("applying as.data.table to Data column of annotationDT")
        annotationDT[, Data := lapply(Data, as.data.table)]
    } else {  
        annotationDT <- pageList
    }
    if (isTRUE(rawAnnotationDT)) {
        if(verbose) print(paste0("Not Parsing, ", header, " returning annotationDT"))
        return(annotationDT)
    }
    # parse the results to a user friendly format
    switch(header,
        'ATC Code'=return(.parseATCannotations(annotationDT)),
        'Drug Induced Liver Injury'=return(.parseDILIannotations(annotationDT)),
        'NSC Number'=return(.parseNSCannotations(annotationDT)),
        'CTD Chemical-Gene Interactions'=return(.parseCTDannotations(annotationDT)),
        'Names and Synonyms'=return(.parseNamesAndSynonyms(annotationDT)),
        'Synonyms and Identifiers'=return(.parseSynonymsAndIdentifiers(annotationDT)),
        'CAS'=return(.parseCASannotations(annotationDT)),
        tryCatch({
            parseFUN(annotationDT)
        },
        error=function(e) {
            .warning(funContext, 'The parseFUN function failed: ', e,
                '. Returning unparsed results instead. Please test the parseFUN
                on the returned data.')
            return(annotationDT)
        })
    )
}
#' @title getPubChemAnnotation
#' @description queries the PubChem PUG-VIEW API to get a single annotation using a CID for a header
#' 
#' @param compound `character(1)` A valid CID to use for the query.
#' @param header `character(1)` A valid header name for the PUG VIEW annotations
#' @param url `character(1)` The URL to perform API queries on. default = 'https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound'
#' @param output `character(1)` The output format. Defaults to 'JSON'.
#' @param timeout_s `numeric(1)` The number of seconds to wait before timing out. Default is 29.
#' @param retries `numeric(1)` The number of times to retry a failed query. Default is 3.
#' @param quiet `logical(1)` Should the function be quiet? Default is TRUE.
#' @param throttleMessage `logical(1)` Should a message be printed when the query is throttled? Default is FALSE.
#' @param query_only `logical(1)` Should this function early return only the encoded query?
#' 
#' @return A `data.table` of resulting annotations.
#' 
#' @export
getPubChemAnnotation <- function(
    compound,
    annotationType = 'ChEMBL ID',
    url = 'https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound',
    timeout_s = 29,
    retries = 3,
    quiet = TRUE,
    throttleMessage = FALSE,
    query_only = FALSE
    ){
        header <- annotationType

        validHeaders <-  c('ChEMBL ID', 'NSC Number', 'Drug Induced Liver Injury', 'DILI', 'CAS', 'ATC Code')

        # make sure type is in the list of valid types
        if(!(header %in% validHeaders)){
            stop(paste0("header must be one of the following: ", paste0(validHeaders, collapse = ", ")))
        }

        # TODO:: add a check to see if the compound is a valid CID or SID
        # TODO:: allow for variaitons of headers due to spelling errors
        if(header == "DILI") header <- "Drug Induced Liver Injury"
        queryURL <- paste0(.buildURL(url, compound, 'JSON'), '?heading=', header)

        if(query_only) return(URLencode(queryURL))
        tryCatch({
            result <- RETRY('GET', URLencode(queryURL), times = retries, quiet = quiet)
        }, error=function(e) {
            print(paste0("Error: ", e$message))
            return(list(compound, "NA"))
        })

        .checkThrottlingStatus(result, throttleMessage = throttleMessage)
        result <- parseJSON(result)    

        if(is.null(result) || is.na(result) || is.null(result$Record)){

            result <- "NA"
        }
        else {
            result <- switch(header,
                'ChEMBL ID'     = .parseCHEMBLresponse(result),
                'NSC Number'    = .parseNSCresponse(result),
                'DILI'          = .parseDILIresponse(result),
                'Drug Induced Liver Injury' = .parseDILIresponse(result),
                'CAS'           = .parseCASresponse(result),
                'ATC Code'      = .parseATCresponse(result))
        }

        result <- list(compound, result)
        names(result) <- c("cid", header)
        return(result)
    }


#' Retrieve PubChem annotations for a given compound
#'
#' This function retrieves PubChem annotations for a given compound using the specified annotations.
#'
#' @param compound The compound for which PubChem annotations are to be retrieved.
#' @param annotations A character vector specifying the annotations to retrieve.
#' @param ... Additional arguments to be passed to getPubChemAnnotation().
#'
#' @return A merged data table containing the PubChem annotations for the specified compound.
#'
#' @examples
#' getPubChemAnnotations(
#'      compound = "36314", 
#'      annotations= c('ChEMBL ID', 'NSC Number', 'Drug Induced Liver Injury'))
#'
#' @export
getPubChemAnnotations <- function(compound, annotations, ...){
    result <- lapply(annotations, .getPubChemAnnotationDT, compound = compound, ...)
    names(result) <- annotations

    # all values of result should be a 2 column data.table with "cid" and the
    # annotation as the column names. 
    # merge all of them together on "cid" and keep all values even if NA or NULL
    Reduce(function(x, y) merge(x, y, by = "cid", all = TRUE), result)

}

#' Function that returns a DT of getPubChemAnnotation results 
#' @importFrom data.table data.table as.data.table merge.data.table last rbindlist
.getPubChemAnnotationDT <- function(compound, annotationType, ...){
    result <- getPubChemAnnotation(compound, annotationType, ...)
    as.data.table(result, na.rm = F)
}



#' Function that parses the results of the PubChem PUG-VIEW API for the CHEMBL ID header
#' @importFrom data.table data.table as.data.table merge.data.table last rbindlist
.parseCHEMBLresponse <- function(result){
    result <- result$Record$Reference$SourceID
    result <- gsub("Compound::", "", result)
    return(result)
}

#' Function that parses the results of the PubChem PUG-VIEW API for the NSC Number header
#' @importFrom data.table data.table as.data.table merge.data.table last rbindlist
.parseNSCresponse <- function(result){
    result <- result$Record$Reference$SourceID[1]
    result <- gsub(" ", "", result)
    return(result)
}

#' Function that parses the results of the PubChem PUG-VIEW API for the DILI header
#' @importFrom data.table data.table as.data.table merge.data.table last rbindlist
.parseDILIresponse <- function(result){
    
    if(length(result$Record$Section) == 0){
        result <- "NA"
    }else{

        dt_ <- as.data.table(result$Record$Section)
        dt_ <- as.data.table(dt_)$Section[[1]]
        dt_ <- as.data.table(dt_)$Section
        dt_ <- as.data.table(dt_)
        dt_ <- as.data.table(dt_)$Information
        section <- as.data.table(dt_)[1:3, "DILI" := paste0(unlist(Name), ":", unlist(Value))]

        # if any of the first 4 rows are NA, remove it 
        section <- section[!is.na(section$DILI)]

        section <- paste0(section[1:3, DILI], collapse= "; ")

        # create a list for each row as Name:Value string with no spaces and no new lines
        reference <- paste0("LTKBID:", result$Record$Reference$SourceID)
        result <- c(section, reference)
        result <- paste0(result, collapse = "; ")
    }
    return(result)
}

#' Function that parses the results of the PubChem PUG-VIEW API for the CAS header
#' @importFrom data.table data.table as.data.table merge.data.table last rbindlist
.parseCASresponse <- function(result){
    result <- result$Record$Reference$SourceID[1]
    return(result)
}

#' Function that parses the results of the PubChem PUG-VIEW API for the ATC Code header
#' @importFrom data.table data.table as.data.table merge.data.table last rbindlist
.parseATCresponse <- function(result){
    if(length(result$Record$Section) == 0){
                result <- "NA"
                
    }else{dt_ <- as.data.table(result$Record$Section)
    dt_ <- as.data.table(dt_)$Section[[1]]
    dt_ <- as.data.table(dt_)$Information
    dt_ <- as.data.table(dt_)$Value
    dt_ <- as.data.table(dt_[[1]])
    result <- paste0("ATC:", dt_$String)
    if(length(result) > 1){
        # result <- paste0(result, collapse = "; ") # this is the old way

        # example: result
        # [1] "ATC:L - Antineoplastic and immunomodulating agents"           
        # [2] "ATC:L01 - Antineoplastic agents"                              
        # [3] "ATC:L01E - Protein kinase inhibitors"                         
        # [4] "ATC:L01EE - Mitogen-activated protein kinase (mek) inhibitors"
        # [5] "ATC:L01EE04 - Selumetinib"   

        # get the number of letters after the colon and before the dash
        # split the string by the colon
        # get the second element

        codes <- lapply(result, function(x) {
            x <- strsplit(x, ":")[[1]][2]
            x <- strsplit(x, "-")[[1]][1]
            x <- nchar(x)
            return(x)
        })
        result <- strsplit(result[which.max(codes)], "-")[[1]][1]
        result <- gsub(" ", "", result)
    }}

    return(result)
}


## These methods further specialize the queryPubChem function to provide
## a simple user interface that does not require knowledge of the PubChem
## REST API to use.


#' @title getPubChemFromNSC
#'
#' @description
#' Return a data.table mapping from ids to the information specified in `to`.
#'
#' @param ids A `character` or `numeric` vector of valid NSC ids to use for the
#'   query.
#' @param to A `character(1)` vector with the desired return type. Currently
#'   only 'cids' and 'sids' are implemented, but other options are available
#'   via the PubChem API. This corresponds to the `operation` portion of the
#'   PubChem API URL Path.
#' @param ... Fall through arguments to bpmapply. Use this to pass in BPPARAM
#'   parameter to customize parellization settings. Alternatively, just call
#'   `register()` with your desired parallel backend configuration.
#' @param raw A `logical(1)` vector specifying whether to early return the raw
#'   query results. Use this if specifying an unimplemented return to the `to`
#'   parameter.
#'
#' @return A `data.table` where the first column is the specified NSC ids and
#'   the second column is the results specified in `to`.
#'
#' @md
#' @importFrom data.table data.table as.data.table setcolorder
#' @export
getPubChemFromNSC <- function(ids, to='cids', ..., batch=TRUE, raw=FALSE, options=NA, query_only=FALSE) {
    funContext <- .funContext('::getPubChemFromNSC')

    # -- make the GET request
    queryRes <- queryPubChem(ids, domain='substance', ...,
        namespace='sourceid/DTP.NCI', operation=to, batch=batch, raw=raw,
        operation_options=options, query_only=query_only)

    # -- early return option
    if (isTRUE(raw) || isTRUE(query_only)) return(queryRes)

    # -- handle failed queries
    failedQueries <- attributes(queryRes)$failed
    queries <- attributes(queryRes)$queries

    # -- rehandle failed queries, somehow they are getting past queryPubChem?
    ## FIXME:: How are they failed queries not found in queryPubChem?
    failed <- unlist(lapply(queryRes, names)) %in% c("Fault", "Bad", "Error")
    if (any(failed)) {
        newFailedQueries <- Map(list, query=queries[failed], failure=queryRes[failed])
        failedQueries <- c(failedQueries, newFailedQueries)
        queryRes <- queryRes[!failed]
        queries <- queries[!failed]
    }

    # -- process the results
    .replace_NULL_NA <- function(DT) lapply(DT, function(x) {
        ifelse(is.null(x), rep(NA_integer_, length(x)), x) })

    # TODO:: Determine if all results are wrapped in two lists? If not this may
    #>break the function.
    .parseQueryToDT <- function(queryRes) as.data.table(queryRes[[1]][[1]])
    queryRes <- lapply(queryRes, FUN=.parseQueryToDT)
    queryRes <- rbindlist(queryRes, fill=TRUE)
    switch(to,
        'cids'={
            unlistQueryRes <- queryRes[, NSC_id := unlist(queries)][,
                lapply(.SD, FUN=.replace_NULL_NA)][, lapply(.SD, unlist)]
            if (nrow(unlistQueryRes) > nrow(queryRes))
                .warning(funContext, 'Some IDs multimap to returned CIDs,
                    check for sduplicates to see which ones!')
            if (any(is.na(unlistQueryRes$CID))) .warning(funContext, 'Some IDs
                failed to map and will have NA CIDs.')
        },
        'sids'={
            unlistQueryRes <- queryRes[, NSC_id := unlist(queries)][,
                lapply(.SD, FUN=.replace_NULL_NA)][, lapply(.SD, unlist)]
            if (nrow(unlistQueryRes) > nrow(queryRes))
                .warning(funContext, 'Some IDs multimap to returned SIDs,
                    check for duplicates to see which ones!')
            if (any(is.na(unlistQueryRes$SID))) .warning(funContext, 'Some IDs
                failed to map and will have NA SIDs.')
        },
        .error('The operation ', to, ' has not been implemented yet!',
            ' To return the unprocessed results of the query, set `raw=TRUE`.')
    )
    # rearrange columns so that NSC_id is first
    setcolorder(unlistQueryRes, rev(colnames(unlistQueryRes)))
    if (length(failedQueries) > 0) {
        .warning(funContext, 'One or more queries failed, please see
            `attributes(<result>)$failed` for more information.')
        attributes(unlistQueryRes)$failed <- failedQueries
    }
    return(unlistQueryRes)
}
