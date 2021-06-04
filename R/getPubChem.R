

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
#' @seealso [httr::GET], [queryPubChem]
#'
#' @references
#' Kim S, Thiessen PA, Cheng T, Yu B, Bolton EE. An update on PUG-REST: RESTful interface for programmatic access to PubChem. Nucleic Acids Res. 2018 July 2; 46(W1):W563-570. doi:10.1093/nar/gky294.
#' 
#' Kim S, Thiessen PA, Bolton EE, Bryant SH. PUG-SOAP and PUG-REST: web services for programmatic access to chemical information in PubChem. Nucleic Acids Res. 2015 Jul 1; 43(W1):W605-W611. doi: 10.1093/nar/gkv396. 
#' 
#' Kim S, Thiessen PA, Bolton EE. Programmatic Retrieval of Small Molecule Information from PubChem Using PUG-REST. In Kutchukian PS, ed. Chemical Biology Informatics and Modeling. Methods in Pharmacology and Toxicology. New York, NY: Humana Press, 2018, pp. 1-24. doi:10.1007/7653_2018_30.
#'
#' @md
#' @importFrom httr GET
#' @export
getPubChem <- function(id, domain='compound', namespace='cid', operation='',
    output='JSON', ..., url='https://pubchem.ncbi.nlm.nih.gov/rest/pug',
    operation_options='')
{
    # handle list or vector inputs for id
    if (length(id) > 1) id <- paste0(na.omit(id), collapse=',')

    # build query URL
    query <- .buildURL(url, domain, namespace, id, operation, output)
    query <- paste(query, operation_options, sep='?')
    encodedQuery <- URLencode(query, reserved=TRUE)
    print(encodedQuery)

    # get HTTP response
    GET(encodedQuery)
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
parseJSON <- function(response, as='text', ...) {
    fromJSON(content(response, as, ...))
}

#' Query the PubChem REST API, with the result automatically converted from
#'   JSON to a list. This only works when `output='JSON'` in `getPubChem`.
#' 
#' @param ... Fallthrough arguments to `AnnotationGx::getPubChem` function.
#' 
#' @md
#' @export
queryPubChem <- function(...) parseJSON(getPubChem(...))


## ============================
## queryPubChem wrapper methods
## ----------------------------


## These methods further specialize the queryPubChem function to provide
## a simple user interface that does not require knowledge of the PubChem 
## REST API to use.


#' Build a `data.table` of assay ids from the a PubChem query list.
#' 
#' @list
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
            getPubChem(
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




if (sys.nframe() == 0) {
    library(httr)
    library(jsonlite)
    library(data.table)
    library(PharmacoGx)

    GDSC <- readRDS(list.files('../PSets', pattern = 'GDSC.*v2.*', full.names=TRUE))
    drugInfo <- drugInfo(GDSC)

    result <- queryPubChem(id=drugInfo$cid, namespace='cid',
        operation='aids', operation_options='aids_type=active')

    result <- querySynonymsFromName(drugs)
    unlist_result <- lapply(result, unlist)
    expSynDT <- data.table(drugid=drugs, synonyms=unlist_result)
    expSynDT[, synonyms2 := mapply(c, drugid, synonyms)]
    mappedSynonyms <- expSynDT[, .(list(which(..lab_drugids %in% unlist(.SD$synonyms2)))), by=drugid]
    mappedSynonyms <- mappedSynonyms[, unlist(V1), by=drugid]

    unmapped <- setdiff(expSynDT$drugid, mappedSynonyms$drugid)
    unmappedDT <- expSynDT[drugid %in% unmapped]

    # Get assay ids for each cid in drugInfo
    AIDtable <- buildAIDTable(result)

    # 
}