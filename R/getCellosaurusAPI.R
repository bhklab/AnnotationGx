
#' Construct Cellosaurus API URL
#' @keywords internal
.constructCellosaurusURL <- function(query, from, extResource, to, numResults) {
    cellosaurus_api_url <- "https://api.cellosaurus.org/"
    resource <- "search/cell-line?"
    
    if (from == "dr") {
        checkmate::assert_character(extResource)
        from <- paste0(from, ":", extResource)
        q <- paste0("q=", from, ";", query)
    } else {
        q <- paste0("q=", from, ":", query)
    }

    url <- paste0(
        cellosaurus_api_url, resource,
        q,
        "&format=tsv",
        "&fields=", paste(to, collapse = ","),
        "&rows=", numResults
    )

    return(url)
}

#' Search Cellosaurus API
#'
#' This function searches the Cellosaurus API for cell line information based on the provided query.
#'
#' @param query The query string to search for in the Cellosaurus API.
#' @param from The field to search in. Default is "id".
#' @param extResource The external resource to search in. Default is NULL.
#' @param to The fields to return in the response. Default is "id".
#' @param numResults The number of results to return. Default is 1.
#' @param returnURL Logical indicating whether to return the constructed API URL. Default is FALSE.
#'
#' @examples
#' searchCellosaurusAPI("MCF-7")
#'
#' @export
searchCellosaurusAPI <- function(
    query,
    from = "id", 
    extResource = NULL,
    to = c("id", "ac", "ca", "sx", "ag", "di", "derived-from-site",  "misspelling"),
    numResults = 1,
    returnURL = FALSE
){
    checkmate::assert_character(from)
    checkmate::assert_character(query)

    url <- .constructCellosaurusURL(query, from, extResource, to, numResults)
    if(returnURL) return(url)
    response <- httr::RETRY(
        "GET", URLencode(url), times = 5, quiet = TRUE
    )

    response <- httr::content(response, "text")
    response <- data.table::fread(text = response, sep = "\t")
    response <- .parseCellosaurusTSVResponse(response)
    
    response$queryField <- query
    return(response)  
}




#' Parse Cellosaurus TSV Response
#'
#' This function parses the response from the Cellosaurus API and converts it into a data.table.
#'
#' @param response The response from the Cellosaurus API.
#'
#' @return A data.table containing the parsed information from the response.
#'
#' @export
.parseCellosaurusTSVResponse <- function(response) {

    lookup <- c(
        "id" = "Name",
        "ac" = "Accession",
        "ca" = "Category",
        "sx" = "Sex",
        "ag" = "AgeAtSampling",
        "di" = "Disease",
        "dr" = "Cross-References",
        "derived-from-site" = "DerivedFromSite",
        "misspelling" = "Misspellings",
        "ox" = "Organism",
        "cc" = "Comments",
        "hi" = "ParentCellLine",
        "dt" = "Date"
    )

    result <- lapply(
        names(response), 
        function(x) {
            v <- lookup[x]
            if(!x %in% names(lookup)) v <- x
            setNames(list(response[[x]]), v)
        })

    data.table::as.data.table(t(unlist(result)))
}

#' getCellosaurusAccesions Function
#'
#' This function retrieves Cellosaurus accessions for a given set of samples.
#'
#' @param samples A character vector of sample IDs.
#' @param from The source of the sample IDs. Default is "idsy".
#' @param ... Additional arguments to be passed to the searchCellosaurusAPI function.
#' @param threads The number of threads to use for parallel processing. Default is 1.
#'
#' @return A data.table object containing the Cellosaurus accessions for the given samples.
#'
#'
#' @export
getCellosaurusAccesions <- function(samples, from = "idsy", ..., threads=1){
    BPPARAM <- BiocParallel::MulticoreParam(
        workers = threads,
        progressbar = TRUE)

    results <- BiocParallel::bplapply(samples, function(sampleID){
        searchCellosaurusAPI(
            query = sampleID,
            from = from,
            to=c("id", "ac"),
            ...
            )
        },
        BPPARAM = BPPARAM
    )

    # if results is a list of URLS return 
    if(is.character(results[[1]])) return(results)
    
    results <- data.table::rbindlist(results, fill = TRUE)
    results
}


#' Maps Cellosaurus accessions to specified fields
#'
#' This function takes a vector of Cellosaurus accessions and a vector of fields,
#' and retrieves the corresponding information from the Cellosaurus API. It uses
#' multiple cores for parallel processing, with the number of threads specified
#' by the 'threads' parameter.
#'
#' @param accessions A vector of Cellosaurus accessions.
#' @param fields A vector of fields to retrieve for each accession.
#' @param threads The number of threads to use for parallel processing.
#'
#' @return A data.table containing the retrieved information for each accession.
#'
#' @export
mapCellosaursAccessionsToFields <- function(accessions, fields, threads=1){
    BPPARAM <- BiocParallel::MulticoreParam(
        workers = threads,
        progressbar = TRUE)

    results <- BiocParallel::bplapply(accessions, function(accession){
        searchCellosaurusAPI(
            query = accession,
            from = "ac",
            to = fields
        )
    },
    BPPARAM = BPPARAM
    )
    results <- data.table::rbindlist(results, fill = TRUE)
    results
}

# #' Parses the response from the Cellosaurus API.
# #'
# #' This function takes the response from the Cellosaurus API and parses it into a structured format.
# #' It removes unnecessary header lines, splits the response into individual records, and extracts the relevant fields.
# #' The resulting data is returned as a data.table object.
# #'
# #' @param response The response from the Cellosaurus API.
# #' @return A data.table object containing the parsed data.
# .parseCellosaurusTXTResponse <- function(response){

#     response <- strsplit(response, split = "\n")[[1]][-(1:15)]
#     response <- split(response, cumsum(response == "//"))
#     response <- lapply(response, function(x) x[x != "//"])
#     response <- unname(response[lengths(response) > 0])
#     response <- lapply(response, function(i){
#         unlist(lapply(i, .parseCellosarusField))})
    
#     response
#     data.table::rbindlist(lapply(response, function(i) {
#         temp <- t(i)

#         temp <- data.table::as.data.table(temp, keep.rownames = TRUE)
#         # if there are multiple columns with the same name, 
#         # collapse them into a single column separated by "; "
#         }),
#         fill=TRUE)
# }

# #' Parses a single field from the Cellosaurus API response.
# #' @keywords internal
# .parseCellosarusField <- function(field) {
#     res <- strsplit(field, split = "   ")[[1]]
#     setNames(list(res[2]), res[1])
# }


# Write testing code here, this is only executed if the file is run as a script
# It is equivalent to if __name__ == "__main__" in Python
# What it actually does is count the number of stack frames
if (sys.nframe() == 0) {

    samples <- c("HeLa", "22rv1")

    # getCellosaurusAccesions(samples, threads = 2)
    result <- searchCellosaurusAPI("HeLa", returnURL = F)
    # print(result)
    print(result)
}

