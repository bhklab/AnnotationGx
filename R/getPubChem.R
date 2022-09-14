
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
#' @details
#' See https://pubchemdocs.ncbi.nlm.nih.gov/pug-rest for the complete API
#'   documentation. A subset of this documentation is included below for
#'   convenience.
#'
#' ## URL Path
#'
#' Most – if not all – of the information PubChem PUG service needs to
#'   produce its results is encoded into the URL. The general form of the URL
#'   has three parts – input, operation, and output – after the common prefix,
#'   followed by operation options as URL arguments (after the ‘?’):
#'
#' https://pubchem.ncbi.nlm.nih.gov/rest/pug/<input specification>/<operation specification>/[<output specification>][?<operation_options>]
#'
#' ### Input
#'
#' The input portion of the URL tells the service which records to use as the
#'   subject of the query. This is further subdivided into two or more locations
#'   in the URL “path” as follows:
#'
#' <input specification> = <domain>/<namespace>/<identifiers>
#'
#' <domain> = substance | compound | assay | <other inputs>
#'
#' compound domain <namespace> = cid | name | smiles | inchi | sdf | inchikey | formula | <structure search> | <xref> | listkey | <fast search>
#'
#' substance domain <namespace> = sid | sourceid/<source id> | sourceall/<source name> | name | <xref> | listkey
#'
#' assay domain <namespace> = aid | listkey | type/<assay type> | sourceall/<source name> | target/<assay target> | activity/<activity column name>
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
#' compound domain <operation specification> = record | <compound property> | synonyms | sids | cids | aids | assaysummary | classification | <xrefs> | description | conformers
#'
#' substance domain <operation specification> = record | synonyms | sids | cids | aids | assaysummary | classification | <xrefs> | description
#'
#' assay domain <operation specification> = record | concise | aids | sids | cids | description | targets/<target type> | <doseresponse> | summary | classification
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
#' <output specification> = XML | ASNT | ASNB | JSON | JSONP [ ?callback=<callback name> ] | SDF | CSV | PNG | TXT
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
#' @param proxy `logical(1)` Should a random proxy server be used for the
#'   get request. Default is `FALSE`. This is useful to avoid getting
#'   black-listed from the API.
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
#' @importFrom httr RETRY GET timeout use_proxy
#' @importFrom data.table data.table fread
#' @importFrom crayon strip_style
#' @export
getRequestPubChem <- function(id, domain='compound', namespace='cid', operation=NA,
        output='JSON', ..., url='https://pubchem.ncbi.nlm.nih.gov/rest/pug',
        operation_options=NA, proxy=FALSE, raw=FALSE, query_only=FALSE) {
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
        if (isTRUE(proxy)) {
            proxyDT <- AnnotationGx:::proxyManager$get_proxies()
            result <- FALSE
            count <- 1
            while(isFALSE(result)) {
                proxy <- unlist(proxyDT[sample(.N, 1), ])
                result <- tryCatch({
                    RETRY('GET', encodedQuery, timeout(29), times=3, quiet=TRUE,
                        terminate_on=c(400, 404, 503),
                        use_proxy(proxy[1], port=as.integer(proxy[2])))
                }, error=function(e) FALSE)
                count <- count + 1
                if (count > 10) .error(funContext, 'Infinite retry loop
                    due to failed proxy requests!')
            }
        } else {
            result <- RETRY('GET', encodedQuery, timeout(29), times=3,
                quiet=TRUE, terminate_on=c(400, 404, 503))
        }

        if (isTRUE(raw)) return(result)

        .checkThrottlingStatus(result)

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
                            Details=paste0(strip_style(e), collapse=' ')))),
                    status_code=400)
            }
        })
}

#' Checks to see if the PubChem query is exceeding the throttling limit
#' @param response `httr::response`
.checkThrottlingStatus <- function(response) {
    throttling_control <- headers(response)$`x-throttling-control`
    any_grepl <- function(...) any(grepl(...))
    throttling_state <- max(which(vapply(
        c('Green', 'Yellow', 'Red', 'Black', 'blacklisted'),
        FUN=any_grepl, x=throttling_control, FUN.VALUE=logical(1))))
    if (throttling_state == 2) {
        .warning('PubChem Server returned Yellow status! Sleeping to compensate.')
        Sys.sleep(5)
    } else if (throttling_state == 3) {
        .warning('PubChem Server returend Red status! Sleeping to compensate.')
        Sys.sleep(10)
    } else if (throttling_state == 4) {
        .error('PubChem Server returned Black status! You could be ',
            'black listed. The returned state message is: ',
            throttling_control, '.')
    } else if (throttling_state == 5) {
        .error('PubChem server indicated: too many queries per second',
            ' or you may be blacklisted.')
    }
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
        operation_options=NA, batch=TRUE, raw=FALSE, proxy=FALSE,
        query_only=FALSE) {
    if (!is.character(id)) id <- as.character(id)
    if (namespace %in% c('name', 'xref', 'smiles', 'inchi', 'sdf'))
        batch <- FALSE

    # Cap parallelization at 5 cores to prevent excessive requests
    BPPARAM <- list(...)[['BPPARAM']]
    if (is.null(BPPARAM)) {
        BPPARAM <- bpparam()
        if (class(BPPARAM) %in% c('MulticoreParam', 'SnowParam')) {
            if (isFALSE(proxy) && bpnworkers(BPPARAM) > 5)
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
            operation_options=operation_options, BPPARAM=BPPARAM, proxy=proxy,
            raw=raw, query_only=query_only)
    } else {
        queryRes <- bplapply(id, FUN=.queryPubChemSleep, domain=domain,
            namespace=namespace, operation=operation, output=output, url=url,
            operation_options=operation_options, BPPARAM=BPPARAM, proxy=proxy,
            raw=raw, query_only=query_only)
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

## TODO:: Retrieve PubChem server status to dynamically set query spacing
##>based on server load
## TODO:: Make the query away for server load status in response header
#' @importFrom crayon strip_style
.queryPubChemSleep <- function(x, ..., query_only=FALSE) {
    proxy <- list(...)$proxy
    t1 <- Sys.time()
    queryRes <- tryCatch({
        queryRequestPubChem(x, ..., query_only=query_only)
    },
        error=function(e) {
            cat('\r')
            print(e)
        list(Error=list(
            Code='.queryPubChemSleep.ERROR',
            Message='See Details for error msg',
            Details=paste0(strip_style(e), collapse=' ')
        ))
    })
    t2 <- Sys.time()
    queryTime <- t2 - t1
    if (!isTRUE(proxy) && queryTime < 0.31) Sys.sleep(0.31 - queryTime)
    return(queryRes)
}

#' Parse a JSON into a list
#'
#' @param response A `response` object as returned by `httr::GET`
#' @param as A `character` vector indicating the return type. Options are 'raw',
#    'text' or 'parsed'. Default is 'text'.
#' @param ... Additional arguments to the `httr::content` function.
#'
#' @seelalso [httr::content]
#'
#' @md
#' @importFrom jsonlite fromJSON
#' @importFrom httr content
#' @export
parseJSON <- function(response, ..., encoding='UTF-8', query_only=FALSE) {
    if (isTRUE(query_only)) return(response)
    tryCatch({
        fromJSON(content(response, ..., as='text', type='JSON',
            encoding=encoding))
    },
    error=function(e) {
        fromJSON(content(response, ..., type='JSON', encoding=encoding))
    })
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


## ============================
## queryPubChem wrapper methods
## ----------------------------


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
#' @param proxy `logical(1)` Should the query be routed through a random
#'   proxy server. This is useful to keep trying queries if a user gets
#'   blacklisted.
#'
#' @return A `data.table` where the first column is the specified NSC ids and
#'   the second column is the results specified in `to`.
#'
#' @md
#' @importFrom data.table data.table as.data.table setcolorder
#' @export
getPubChemFromNSC <- function(ids, to='cids', ..., batch=TRUE, raw=FALSE,
        proxy=FALSE, options=NA, query_only=FALSE) {
    funContext <- .funContext('::getPubChemFromNSC')

    # -- make the GET request
    queryRes <- queryPubChem(ids, domain='substance', ...,
        namespace='sourceid/DTP.NCI', operation=to, batch=batch, raw=raw,
        proxy=proxy, operation_options=options, query_only=query_only)

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
#' @param proxy `logical(1)` Route API queries through random proxy servers?
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
    properties='Title', batch=TRUE, raw=FALSE, proxy=FALSE, options=NA,
    query_only=FALSE)
{
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

    if (to == 'property')
        to <- paste0(to, '/', paste0(properties, collapse=','))
    queryRes <- queryPubChem(ids, domain='compound', namespace=from,
        operation=to, batch=batch, raw=raw, proxy=proxy,
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


#' @title getPubChemSubstance
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
#' @param batch `logical(1)` Should the query be run in batches (i.e., multiple
#'   ids per GET request to the API). Default is `TRUE`. Should be set to
#'   `FALSE` when retrying failed queries. Batch queries are not supported when
#'   `from` is one of 'name', 'xref', 'smiles', 'inchi' or 'sdf'. In these
#'   cases, batch will automatically be set to `FALSE` with a warning.
#' @param raw `logical(1)` Should the raw query results be early returned. This
#'   can be useful for diagnosing issues with failing queries.
#' @param proxy
#'
#' @return A `data.frame` or `list` containing results of the query.
#'
#' @md
#' @importFrom data.table setnames as.data.table rbindlist
#' @export
getPubChemSubstance <- function(ids, from='cid', to='sids', ...,
    batch=TRUE, raw=FALSE, proxy=FALSE)
{
    if (!is.character(ids)) ids <- as.character(ids)
    if (from %in% c('name', 'xref', 'smiles', 'inchi', 'sdf')) {
        if (isTRUE(batch)) .warning('Batch queries cannot be used when mapping
            from name, xref, smiles, inchi or sdf. Setting to batch=FALSE.')
        batch <- FALSE
    }
    queryRes <- queryPubChem(ids, domain='substance',
        namespace=from, operation=to, batch=batch, raw=raw, proxy=proxy, ...)

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
#' @importFrom httr GET use_proxy
#' @importFrom jsonlite fromJSON
#' @importFrom data.table data.table as.data.table merge.data.table last rbindlist fwrite
#' @importFrom BiocParallel bpparam bpworkers bpprogressbar bptry
#' @export
getPubChemAnnotations <- function(header='Available', type='Compound',
    parseFUN=identity, ..., output='JSON', raw=FALSE,
    url='https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/annotations/heading',
    BPPARAM=bpparam(), proxy=FALSE)
{
    funContext <- .funContext('::getPubChemAnnotations')
    if (header == 'Available') {
        queryURL <-
            'https://pubchem.ncbi.nlm.nih.gov/rest/pug/annotations/headings/JSON'
    } else {
        queryURL <- paste0(.buildURL(url, header, output),
            '?heading_type=', type)
    }
    encodedQueryURL <- URLencode(queryURL)

    if (isTRUE(proxy)) {
        proxyDT <- fread(file.path(tempdir(), 'proxy.csv'))
        queryRes <- FALSE
        count <- 1
        while(isFALSE(queryRes)) {
            proxy <- unlist(proxyDT[sample(.N, 1), ])
            queryRes <- tryCatch({ RETRY('GET', encodedQueryURL, timeout(29), times=3,
                quiet=TRUE, use_proxy(proxy[1], as.integer(proxy[2])))
            }, error=function(e) FALSE)

            if (isFALSE(queryRes)) {
                proxyDT <- proxyDT[ip != proxy[1] & port != proxy[2], ]
            }
            count <- count + 1
            if (count > 10) .error(funContext, 'Infinite retry loop
                due to failed proxy requests!')
        }
        fwrite(proxyDT, file=file.path(tempdir(), 'proxy.csv'))
    } else {
        queryRes <- RETRY('GET', encodedQueryURL, timeout(29), times=3,
            quiet=TRUE)
    }

    if (isTRUE(raw)) return(queryRes)

    resultDT <- as.data.table(parseJSON(queryRes)[[1]][[1]])

    if (header == 'Available') return(resultDT)

    numPages <- as.numeric(content(queryRes)[[1]]$TotalPages)
    if (numPages > 1) {
        tryCatch({
            bpworkers(BPPARAM) <- 5
            bpprogressbar(BPPARAM) <- TRUE
        }, error=function(e) .warning(funContext, 'Failed to set parallelzation
            parameters! Please configure them yourself and pass in as the
            BPPARAM argument.'))
        pageList <- bplapply(seq(2, numPages), function(i, queryURL, numPages) {
            t1 <- Sys.time()
            encodedURL <- URLencode(paste0(queryURL, '&page=', i))
            if (isTRUE(proxy)) {
                proxyDT <- fread(file.path(tempdir(), 'proxy.csv'))
                queryRes <- FALSE
                count <- 1
                while(isFALSE(queryRes)) {
                    proxy <- unlist(proxyDT[sample(.N, 1), ])
                    print(proxy)
                    queryRes <- tryCatch({ RETRY('GET', encodedQueryURL, timeout(29), times=3,
                        quiet=TRUE, use_proxy(proxy[1], as.integer(proxy[2])))
                    }, error=function(e) FALSE)

                    if (isFALSE(queryRes)) {
                        proxyDT <- proxyDT[ip != proxy[1] & port != proxy[2], ]
                    }
                    count <- count + 1
                    if (count > 10) .error(funContext, 'Infinite retry loop
                        due to failed proxy requests!')
                }
                fwrite(proxyDT, file=file.path(tempdir(), 'proxy.csv'))
            } else {
                queryRes <- RETRY('GET', encodedQueryURL, timeout(29), times=3,
                    quiet=TRUE)
            }
            page <- tryCatch({
                as.data.table(parseJSON(queryRes)[[1]][[1]])
            }, error=function(e) {
                .warning(funContext, 'Parsing to JSON failed! Returning empty
                    data.table.')
                return(data.table())
            })
            t2 <- Sys.time()
            queryTime <- t2 - t1
            if (queryTime < 0.31) Sys.sleep(0.31 - queryTime)
            return(page)
        }, BPPARAM=BPPARAM, queryURL=queryURL, numPages=numPages)
        pageList <- c(list(resultDT), pageList)
    } else {
        pageList <- list(resultDT)
    }

    if (header != 'CAS') {
        annotationDT <- rbindlist(pageList, fill=TRUE, use.names=TRUE)
        annotationDT[, Data := lapply(Data, as.data.table)]
    }

    # parse the results to a user friendly format
    switch(header,
        'ATC Code'=return(.parseATCannotations(annotationDT)),
        'Drug Induced Liver Injury'=return(.parseDILIannotations(annotationDT)),
        'NSC Number'=return(.parseNSCannotations(annotationDT)),
        'CTD Chemical-Gene Interactions'=return(.parseCTDannotations(annotationDT)),
        'Names and Synonyms'=return(.parseNamesAndSynonyms(annotationDT)),
        'Synonyms and Identifiers'=return(.parseSynonymsAndIdentifiers(annotationDT)),
        'CAS'=return(.parseCASannotations(pageList)),
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

# -----------------------------
# getPubChemAnnotations Helpers

#' @importFrom data.table data.table as.data.table merge.data.table last rbindlist
.parseATCannotations <- function(DT) {
    dataL <- DT$Data
    names(dataL) <- DT$SourceID
    dataDT <- rbindlist(dataL, fill=TRUE, use.names=TRUE, idcol='SourceID')
    dataDT[, ATC_code := unlist(lapply(Value.StringWithMarkup,
        function(x) last(x)[[1]]))]
    annotationDT <- merge.data.table(
        dataDT[, .(SourceID, ATC_code)],
        DT[, .(SourceName, SourceID, LinkedRecords)],
        by='SourceID'
    )
    DT <- annotationDT[, .(CID=unlist(LinkedRecords)),
        by=.(SourceName, SourceID, ATC_code)]
    return(DT)
}

#' @importFrom data.table data.table as.data.table merge.data.table last rbindlist
.parseDILIannotations <- function(DT) {
    dataL <- DT$Data
    names(dataL) <- DT$SourceID
    dataL <- lapply(dataL, FUN=`[`, i=Name %like% 'DILI')
    dataDT <- rbindlist(dataL, fill=TRUE, use.names=TRUE, idcol='SourceID')
    dataDT[, DILI := unlist(Value.StringWithMarkup)]
    annotationDT <- merge.data.table(
        dataDT[, .(SourceID, DILI)],
        DT[, .(SourceID, SourceName, Name, LinkedRecords.CID,
            LinkedRecords.SID)],
        by='SourceID')
    DT <- annotationDT[, .(CID=unlist(LinkedRecords.CID), SID=unlist(LinkedRecords.SID)),
        by=.(SourceName, SourceID, Name, DILI)]
    return(DT)
}

#' @importFrom data.table data.table as.data.table merge.data.table last rbindlist
.parseNSCannotations <- function(DT) {
    DT[, NSC := unlist(lapply(Data, `[[`, i=4))]
    annotationDT <- DT[,
        .(CID=unlist(LinkedRecords.CID), SID=unlist(LinkedRecords.SID)),
        by=.(SourceName, SourceID, NSC)]
    return(annotationDT)
}

#' @importFrom data.table data.table as.data.table merge.data.table last rbindlist
.parseCTDannotations <- function(DT) {
    annotationDT <- DT[, .(CID=unlist(LinkedRecords)),
        by=.(SourceName, SourceID, URL)]
    annotationDT[, CTD := gsub('::.*$', '', SourceID)]
    return(annotationDT)
}

#' @importFrom data.table data.table as.data.table merge.data.table last rbindlist setnames
.parseCASannotations <- function(list) {
    # Make sure CIDs all go in the same column
    CAS_list <- lapply(list, setnames, old='LinkedRecords.CID', new='LinkedRecords',
        skip_absent=TRUE)
    DT <- rbindlist(CAS_list, fill=TRUE, use.names=TRUE)
    DT[, CAS := lapply(Data, function(x) unlist(x[[2]]))]
    CAS_DT <- DT[, .(CAS=unlist(CAS)), by=.(SourceName, SourceID, Name)]
    ID_DT <- DT[, .(
        CID=unlist(lapply(LinkedRecords, function(x) if(is.null(x)) NA_integer_ else x)),
        SID=unlist(lapply(LinkedRecords.SID, function(x) if(is.null(x)) NA_integer_ else x)))
        , by=.(SourceName, SourceID, Name, URL)]
    annotationDT <- merge.data.table(CAS_DT, ID_DT,
        by=c('SourceName', 'SourceID', 'Name'), all.x=TRUE)
    return(annotationDT)
}

#' @importFrom data.table data.table as.data.table merge.data.table last rbindlist
.parseSynonymsAndIdentifiers <- function(DT) {
    dataList <- lapply(DT$Data, as.data.table)
    names(dataList) <- DT$SourceID
    dataDT <- rbindlist(dataList, fill=TRUE, use.names=TRUE,
        idcol='SourceID')
    DT[, Data := NULL]
    dataDT[,
        Synonyms := paste0(unlist(Value.StringWithMarkup[[1]]), collapse='|'),
        by=SourceID]
    dataDT[, Synonyms := paste(Synonyms, '|', Name), by=SourceID]
    dataDT[, Value.StringWithMarkup := NULL]
    annotationDT <- merge.data.table(dataDT, DT, by='SourceID')
    setnames(annotationDT,
        old=c('TOCHeading.type', 'TOCHeading..TOCHeading', 'LinkedRecords'),
        new=c('Type', 'Heading', 'ID')
    )
    return(annotationDT)
}

#' @importFrom data.table data.table as.data.table merge.data.table last rbindlist
.parseNamesAndSynonyms <- function(DT) {
    DT[, Synonyms := lapply(Data, function(x) x[2, ]$Value[[1]][[1]])]
    # Remove the weird annotation from the end of the synonym
    DT[, Synonyms := lapply(Synonyms, FUN=gsub, pattern=' - .*$', replacement='')]
    DT[, Synonyms := unlist(lapply(Synonyms, FUN=paste0, collapse='|'))]
    # fix NULL list itemss
    DT[, CID := lapply(LinkedRecords, function(x) if(is.null(x)) NA_integer_ else x)]
    annotationDT <- DT[, .(CID=unlist(CID)),
        by=.(SourceName, SourceID, Name, URL, Synonyms)]
    return(annotationDT)
}
