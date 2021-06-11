
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
#' @param ... Force subsequent arguments to be named. Not used.
#' @param url The URL of the PubChem REST API. Probably don't change this.
#' @param operation_options Further optional arguments for the selected operation.
#'   this is specific to the selected operation. This is appended as a string
#'   after '?' at the end of the query. See 
#'   https://pubchemdocs.ncbi.nlm.nih.gov/pug-rest$_Toc494865565 for details.
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
#' @importFrom httr RETRY GET timeout
#' @export
getRequestPubChem <- function(id, domain='compound', namespace='cid', operation=NA,
    output='JSON', ..., url='https://pubchem.ncbi.nlm.nih.gov/rest/pug',
    operation_options=NA)
{
    # handle list or vector inputs for id
    if (length(id) > 1) id <- paste0(na.omit(id), collapse=',')

    # replace special characters in id
    id <- URLencode(id, reserved=TRUE)

    # build query URL
    query <- .buildURL(url, domain, namespace, id, operation, output)
    if (!is.na(operation_options))
        query <- paste(query, operation_options, sep='?')
    encodedQuery <- URLencode(query)

    # print(dput(encodedQuery))

    # get HTTP response, respecting the 30s max query time of PubChem API
    tryCatch(RETRY('GET', encodedQuery, timeout(29), times=5),
        error={function(e) message("GET error: ", e)})
}

#' @title queryPubChem
#'
#' @inheritParams getRequestPubChem
#' @param ... Fall through parameters to `bpmapply`.
#'
#' @md
#' @export
queryPubChem <- function(id, domain='compound', namespace='cid', operation=NA,
    output='JSON', ..., url='https://pubchem.ncbi.nlm.nih.gov/rest/pug',
    operation_options=NA, batch=TRUE, raw=FALSE)
{
    if (!is.character(id)) id <- as.character(id)
    
    # Cap parallelization at 5 cores to prevent excessive requests
    BPPARAM <- list(...)[['BPPARAM']]
    if (is.null(BPPARAM)) {
        BPPARAM <- bpparam()
        if (class(BPPARAM) %in% c('MulticoreParam', 'SnowParam')) {
            if (bpnworkers(BPPARAM) > 5) bpworkers(BPPARAM) <- 5  
        }
        bpprogressbar(BPPARAM) <- TRUE
    }

    ## TODO:: Retrieve PubChem server status to dynamically set query spacing
    ##>based on server load
    .queryPubChemSleep <- function(x, i, ...) {
        if (is(list(...)[['BPPARAM']], 'SerialParam')) Sys.sleep(0.17)
        queryRequestPubChem(x, ..., BPPARAM=BPPARAM)
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

        queryRes <- bpmapply(FUN=.queryPubChemSleep, x=queries, i=seq_along(queries),
            MoreArgs=list(domain=domain, namespace=namespace, operation=operation,
                output=output, url=url, operation_options=operation_options), 
            SIMPLIFY=FALSE, BPPARAM=BPPARAM) 
    } else {
        queryRes <- bpmapply(FUN=.queryPubChemSleep, x=id, i=seq_along(id),
            MoreArgs=list(domain=domain, namespace=namespace, operation=operation,
                output=output, url=url, operation_options=operation_options),
            SIMPLIFY=FALSE, BPPARAM=BPPARAM)
        queries <- as.list(id)
    }

    # -- early return option
    if (raw)  return(queryRes)   

    # -- deal with failed queries
    failed <- unlist(lapply(queryRes, names)) %in% c("Fault", "Bad")
    failedQueries <- Map(list, query=queries[failed], failure=queryRes[failed])
    queryRes <- queryRes[!failed]
    queries <- queries[!failed]
    if (is.null(queries)) queries <- NA

    # -- Attach query metadata to the returned list
    attributes(queryRes)$failed <- failedQueries
    attributes(queryRes)$queries <- queries

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
parseJSON <- function(response, as='text', ..., encoding='UTF-8') {
    fromJSON(content(response, as=as, ..., encoding=encoding))
}

#' Query the PubChem REST API, with the result automatically converted from
#'   JSON to a list. This only works when `output='JSON'` in `getRequestPubChem`.
#' 
#' @param ... Fallthrough arguments to `AnnotationGx::getRequestPubChem` function.
#' 
#' @md
#' @export
queryRequestPubChem <- function(...) parseJSON(getRequestPubChem(...))


## ============================
## queryPubChem wrapper methods
## ----------------------------


## These methods further specialize the queryPubChem function to provide
## a simple user interface that does not require knowledge of the PubChem 
## REST API to use.


#' Build a `data.table` of assay ids from the a PubChem query list.
#' 
#' @param A
#' 
#' 
#' 
#' 
#' 
#' 
#' 
buildAIDTable <- function(list) {
    as.data.table(list$InformationList$Information)
}

#'
#'
#'
#' 
querySynonymsFromName <- function(drugNames) {
    ## TODO:: Design a general mechanism for correcting special character
    safeDrugNames <- drugNames
    results <- vector(mode='list', length(drugNames))
    names(results) <- safeDrugNames
    # Quote all drug names for safety
    for (drug in safeDrugNames) {
        results[[drug]] <- content(
            getRequestPubChem(
                id=drug, 
                namespace='Name',
                operation='synonyms'),
            'parsed'
        )
        Sys.sleep(0.17) # prevent making more than 5 requests per second
                        # also prevents more than 400 requests per minute
    }
    return(results)
}

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
#' @param raw
#' @param raw A `logical(1)` vector specifying whether to early return the raw
#'   query results. Use this if specifying an unimplemented return to the `to`
#'   parameter.
#'
#' @return A `data.table` where the first column is the specified NSC ids and 
#'   the second column is the results specified in `to`.
#'
#' @md
#' @importFrom data.table data.table as.data.table
#' @importFrom CoreGx .error .warning
#' @importFrom BiocParallel bpmapply bpparam bpnworkers bpworkers<- bpprogressbar<-
#' @export
getPubChemFromNSC <- function(ids, to='cids', ..., batch=TRUE, raw=FALSE) {
    
    funContext <- .funContext('::getPubChemFromNSC')

    # -- make the GET request
    queryRes <- queryPubChem(ids, domain='substance', ...,
        namespace='sourceid/DTP.NCI', operation=to, batch=batch, raw=raw)

    # -- early return option
    if (raw) return(queryRes)

    # -- handle failed queries
    failedQueries <- attributes(queryRes)$failed
    queries <- attributes(queryRes)$queries

    # -- process the results
    .replace_NULL_NA <- function(DT) lapply(DT, function(x) { 
        ifelse(is.null(x), rep(NA_integer_, length(x)), x) })

    # TODO:: Determine if all results are wrapped in two lists? If not this may 
    #>break the function.
    .parseQueryToDT <- function(queryRes) as.data.table(queryRes)[[1]][[1]]
    queryRes <- lapply(queryRes, FUN=.parseQueryToDT)
    queryRes <- rbindlist(queryRes)
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
    if (length(failedQueries) > 0) {
        .warning(funContext, 'One or more queries failed, please see 
            `attributes(<result>)$failed` for more information.')
        attributes(unlistQueryRes)$failed <- failedQueries
    }
    # rearrange columns so that NSC_id is first
    setcolorder(unlistQueryRes, rev(colnames(unlistQueryRes)))
    return(unlistQueryRes)
}

#' @title getPubChemFromCID
#'
#' @param ids A `character` or `numeric` vector of valid PubChem CIDs to use
#'   for the query.
#' @param from A `character(1)` vector with the desired namespace to query.
#'   Default is 'cid'. Try using 'sid' if some of your CIDs fail to map.
#' @param to A `character(1)` vector with the desired return type. Defaults
#'   to 'record', which returns all available data from the specified IDs.
#' @param ... Force subsequent parameters to be named. Not used.
#' @param properties A `character` vector of properties to return. Only used
#'   when `to='property'`. Common properties of interest are: 'Title' (name), 
#'   'IUPACName', 'CanonicalSMILES', 'IsomericSMILES', 'InChIKey'. The default
#'   setting with return all of these.
#'   See details for more information.
#'
#' @return A `data.frame` or `list` containing results of the query.
#'
#' @details
#' ## `properties`
#' For a full list of availabe properties see: 
#' https://pubchemdocs.ncbi.nlm.nih.gov/pug-rest$_Toc494865556
#'
#' @md
#' @export
getPubChemCompound <- function(ids, from='cid', to='property', ..., 
    properties='Title', batch=TRUE, raw=FALSE) 
{
    if (!is.character(ids)) ids <- as.character(ids)
    if (to == 'property')
        to <- paste0(to, '/', paste0(properties, collapse=','))
    queryRes <- queryPubChem(ids, domain='compound', 
        namespace='cid', operation=to, batch=batch, raw=raw)

    # -- early return option
    if (raw) return(queryRes)

    # -- deal with failed queries
    failedQueries <- attributes(queryRes)$failed

    # -- process the results
    .replace_NULL_NA <- function(DT) lapply(DT, function(x) { 
        ifelse(is.null(x), rep(NA_integer_, length(x)), x) })

    # TODO:: Determine if all results are wrapped in two lists? If not this may 
    #>break the function.
    .parseQueryToDT <- function(queryRes) as.data.table(queryRes)[[1]][[1]]
    queryRes <- lapply(queryRes, FUN=.parseQueryToDT)
    queryRes <- rbindlist(queryRes)

    if (to == 'sid') setcolnames(queryRes, 'CID', 'SID')
    if (length(failedQueries) > 1) attributes(queryRes)$failed <- failedQueries
    
    return(queryRes)
}


if (sys.nframe() == 0) {
    library(AnnotationGx)
    library(data.table)

    ids <- unique(na.omit(fread('local_data/DTP_NCI60_RAW.csv')[[1]]))
    NSCtoCID <- getPubChemFromNSC(ids)

    failed <- attributes(NSCtoCID)$failed
    failedQueries <- lapply(failed, FUN=`[[`, i='query')

    retryQueries <- lapply(failedQueries, FUN=getPubChemFromNSC, batch=FALSE)

    cids <- na.omit(NSCtoCID$CID)
    compoundProperties <- getPubChemCompound(cids)

    # GDSC <- readRDS(list.files('../PSets', pattern = 'GDSC.*v2.*', full.names=TRUE))
    # drugInfo <- drugInfo(GDSC)

    # result <- queryPubChem(id=drugInfo$cid, namespace='cid',
    #     operation='aids', operation_options='aids_type=active')

    # result <- querySynonymsFromName(drugs)
    # unlist_result <- lapply(result, unlist)
    # expSynDT <- data.table(drugid=drugs, synonyms=unlist_result)
    # expSynDT[, synonyms2 := mapply(c, drugid, synonyms)]
    # mappedSynonyms <- expSynDT[, .(list(which(..lab_drugids %in% unlist(.SD$synonyms2)))), by=drugid]
    # mappedSynonyms <- mappedSynonyms[, unlist(V1), by=drugid]

    # unmapped <- setdiff(expSynDT$drugid, mappedSynonyms$drugid)
    # unmappedDT <- expSynDT[drugid %in% unmapped]

    # # Get assay ids for each cid in drugInfo
    # AIDtable <- buildAIDTable(result)

    # # 
}