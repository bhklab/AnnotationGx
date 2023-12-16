



# Allowed Fields:
#' id, sy, idsy, ac, acas, dr, ref, rx, ra, rt, rl, 
#' ww, genome-ancestry, hla, registration, sequence-variation, 
#' anecdotal, biotechnology, breed, caution, cell-type, 
#' characteristics, donor-info, 
#' derived-from-site, discontinued, doubling-time, from, group, 
#' karyotype, knockout, msi, miscellaneous, misspelling, mab-isotype, 
#' mab-target, omics, part-of, population, problematic, resistance, 
#' senescence, transfected, transformant, virology, 
#' cc, str, di, din, dio, ox, sx, ag, oi, hi, ch, ca, dt, dtc, dtu, dtv

#' Search Cellosaurus API
#'
#' This function searches the Cellosaurus API for cell line information based on the provided query.
#'
#' @param query The query string to search for.
#' @param from The field to search in. Default is "id".
#' @param extResource The external resource to search in, only applicable when from is "dr".
#' @param format The format of the response. Only "txt" format is currently supported.
#' @param to The fields to include in the response. The default is :
#' "id", "ac", "sy", "ca", "sx", "ag", "din", "derived-from-site",  "misspelling")
#' @param numResults The number of results to return. Default is 1.
#' @param returnURL Logical indicating whether to return the constructed URL instead of making the API request. Default is FALSE.
#'
#' @return A data frame containing the search results.
#'
#' @examples
#' searchCellosaurusAPI("MCF-7")
#'
#' @export
searchCellosaurusAPI <- function(
    query,
    from = "id", 
    extResource = NULL,
    format = "tsv",
    to = c("id", "ac", "ca", "sx", "ag", "di", "derived-from-site",  "misspelling"),
    numResults = 1,
    returnURL = FALSE
){
    if(!format %in% c("txt","tsv")) stop("Only txt,tsvformat is currently supported")

    checkmate::assert_character(from)
    checkmate::assert_character(query)
    checkmate::assert_character(format)

    cellosaurus_api_url <- "https://api.cellosaurus.org/"
    resource <- "search/cell-line?"

    if(from == "dr"){
        checkmate::assert_character(extResource)
        from <- paste0(from, ":", extResource)
        q <- paste0("q=", from, ";", query)
    }else{
        q <- paste0("q=", from, ":", query)
    }

    url <- paste0(
        cellosaurus_api_url, resource,
        q,
        "&format=", format,
        "&fields=", paste(to, collapse=","),
        "&rows=", numResults)

    if(returnURL) return(url)
    response <- httr::GET(URLencode(url))

    if(format == "tsv"){
        response <- httr::content(response, "text")
        response <- data.table::fread(text = response, sep = "\t")
        response <- .parseCellosaurusTSVResponse(response)
    }else if(format == "txt"){
        response <- httr::content(response)
        response <- .parseCellosaurusTXTResponse(response)
    }
    response <- response[, "queryField" := query]

    return(response)  
}


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
#' @examples
#' getCellosaurusAccesions(samples = c("sample1", "sample2"), from = "idsy", threads = 2)
#'
#' @import BiocParallel
#' @import data.table
#' @export
getCellosaurusAccesions <- function(samples, from = "idsy", ..., threads=1){
    results <- BiocParallel::bplapply(samples, function(sampleID){
        searchCellosaurusAPI(
            query = sampleID,
            from = from, 
            to = c("ac"),
            format = "txt",
            ...
            )
        },
        BPPARAM = BiocParallel::MulticoreParam(workers = threads)
    )

    # if results is a list of URLS return 
    if(is.character(results[[1]])) return(results)
    
    results <- data.table::rbindlist(results, fill = TRUE)
    results
}

.parseCellosaurusTXTResponse <- function(response){
    response <- strsplit(response, split = "\n")[[1]][-(1:15)]
    response <- split(response, cumsum(response == "//"))
    response <- lapply(response, function(x) x[x != "//"])
    response <- unname(response[lengths(response) > 0])
    response <- lapply(response, function(i){
        unlist(lapply(i, .parseCellosarusField))})
    
    response
    data.table::rbindlist(lapply(response, function(i) {
        temp <- t(i)

        temp <- data.table::as.data.table(temp, keep.rownames = TRUE)
        # if there are multiple columns with the same name, 
        # collapse them into a single column separated by "; "
        }),
        fill=TRUE)
}
.parseCellosarusField <- function(field) {
    res <- strsplit(field, split = "   ")[[1]]
    setNames(list(res[2]), res[1])
}



# #' Create a list of query URLS for Cellosaurus API
# #'
# #' @description
# #' This function creates a queryURL for the cellosaurus API using a list of cell line names
# #'
# #' @details 
# #' Function to create a URL query for Cellosaurus to search for a cell-line using its name 
# #' An example call:   computedURLs <- .createQueryURLs(api = "https://api.cellosaurus.org/", 
# #' cl_names = c("22rv1", "Hela"), fields = c("id", "ac"))
# #' @return A list of URLS
# #' @param api is the link to the API to build the URL. i.e "https://api.cellosaurus.org/"
# #' @param cl_names is a list of the cell line names
# #' @param format is the type of format to return from the API. Can be "txt" or "json" 
# #' @param num_results is the number of of items to return, DEFAULT=1
# #' @param GETfxn is the function to use on the cellosaurus website. Currently only supports "search/cell-line?"
# #' @param fields is a list of desired fields to include in the response 
# #' 
# #' @md
# #' @export
# .createQueryURLs <- 
#     function(api = "https://api.cellosaurus.org/",
#              cl_names,
#              format = "txt",
#              num_results = 1,
#              GETfxn = c("search/cell-line?", "cell-line/"),
#              fields,
#              q = "idsy:") {
        
#         if (GETfxn == "search/cell-line?") {
#                 # create urls 
#             computedURLs <- paste0(
#                 api,
#                 GETfxn,
#                 "q=", q,
#                 gsub(" ", "%20",cl_names),
#                 "&rows=", num_results,
#                 "&format=", format,
#                 "&fields=", paste(fields, collapse=",")
#             )
#             return(computedURLs)
#         } else if (GETfxn == "cell-line/") {
#             computedURLs <- paste0(
#                 api,
#                 GETfxn,
#                 gsub(" ", "%20",cl_names),
#                 "?",
#                 "format=",format,
#                 "&fields=", paste(fields, collapse=",")
#             )
#             return(computedURLs)
#         } else {
#             stop("GETfxn must be either 'search/cell-line?' or 'cell-line/'")
#         }
        
#         return(computedURLs)
# }

# #' Query Cellosaurus
# #' 
# #' @description
# #' This function takes a list of cell line names and interested fields and gets responses from the Cellosaurus API 
# #'
# #' @details 
# #' Function to get responses from Cellosaurus API
# #' 
# #' @return A list of responses
# #' @param cl_names is a list of the cell line names
# #' @param fields is a list of desired fields to obtain for each cell line in the API query, i.e if only trying to get synonynms and primary accesssion then fields=c("sy", "ac"). see https://api.cellosaurus.org/static/fields_help.html for all fields.
# #'  
# #' @md
# #' @export
# #' 
# getCellosaurusAPI <-
#     function(
#             cl_names,                       # List of cell line names 
#             fields = c(
#                 "id",               # Recommended name. Most frequently the name of the cell line as provided in the original publication.
#                 "ac",           # Primary accession. It is the unique identifier of the cell line. It is normally stable across Cellosaurus versions ...
#                 "sy",           # List of synonyms.
#                 "misspelling",  # Identified misspelling(s) of the cell line name
#                 "din",          # Disease(s) suffered by the individual from which the cell line originated with its NCI Thesaurus or ORDO identifier.
#                 "ca",           # Category to which a cell line belongs, one of 14 defined terms. Example: cancer cell line, hybridoma, transformed cell line.
#                 "sx",           # Sex
#                 "ag",           # Age at sampling time of the individual from which the cell line was established.
#                 "sampling-site", # Body part, organ, cell-type the cell line is derived from
#                 "metastatic-site" # Body part, organ the cell line is derived from in the case of a cell line originating from a cancer metastasis.
#                 ),
#             GETfxn = c("search/cell-line?", "cell-line/"), # Function to use on the cellosaurus website
#             querydomain = "ac:"
#             ){
#         cellosaurus_api_url <- "https://api.cellosaurus.org/"
        
#         computedURLs <- .createQueryURLs(api = cellosaurus_api_url, GETfxn = GETfxn, cl_names = cl_names, fields = fields, q = querydomain)

#         responseList <- BiocParallel::bplapply(computedURLs, function(x) GET(x)) 
#         names(responseList) <- cl_names
#         return(responseList)
# }

# #' Clean cellosaurus responses
# #'
# #' @description
# #' This function takes a list of Cellosaurus Responses and cleans them for use 
# #'
# #' @details This function takes a list of Cellosaurus Responses and cleans them for use 
# #' @return A list of responses
# #' @param responseList is a list of responses
# #' 
# #' @md
# #' @export
# #' 
# cleanCellosaurusResponse <- 
#     function(
#         responseList, 
#         GETfxn = c("search/cell-line?", "cell-line/")
#         ){
#         # Get content of each response, then separate content on newline character
#         responseContent <- lapply(lapply(responseList, httr::content),
#                                     function(x) strsplit(x=x, split="\n"))
        
#         if (GETfxn == "search/cell-line?") {
#             #Remove first 15 rows of content
#             responseContent_sub <- lapply(responseContent, function(x) x[[1]][-(1:15)]) 
#             # Split on first "  " appearance
#             responseContent_sub_split <- lapply(responseContent_sub, function(x) strsplit(x, split = "  ."))
#                     # rbind responses (do.call returns as one large matrix instead of dataframe)
#             df_ <- lapply(responseContent_sub_split, function(x) do.call(rbind, x))

#             # convert each response from matrix to data.table
#             df_2 <- lapply(df_, function(x) data.table(x))

#             # rbinds all responses (creates new column so all of cell line x will have its name in col cellLine)
#             df_3<- rbindlist(df_2, idcol = "cellLine")
#             df_3 <- df_3[V1!="//"]

#             # Collapse all rows with the same cellLine & V1 (most often rows for cc) and separate by "; "
#             df_4 <- df_3[, list(data = paste0(unique(na.omit(V2)), collapse ="; ")), by = c("cellLine", "V1")]

#             # transpose 
#             df_5 <- data.table::dcast(df_4, cellLine ~ ...)

#             return(df_5) 
#         } else if (GETfxn == "cell-line/") {
#             #Remove first 15 rows of content
#             # responseContent_sub <- lapply(responseContent, function(x) x[[1]][-(1:15)])
#             responseContent_sub <- responseContent
#             responseContent_sub_split <- lapply(responseContent_sub, function(x) strsplit(x[[1]], split = "  ."))

#             result <- rbindlist(lapply(responseContent_sub_split, function(x) {
#                 # remove the entire column if any of the elements has "//" in it
#                 if (any(grepl("//", x))) {
#                     x <- x[, -1]
#                 }

#                 cvcl_dt <- as.data.table(x)
#                 names(cvcl_dt) <- as.character(cvcl_dt[1])
#                 cvcl_dt[2]
#             }))
#             return(result)

#         } else {
#             stop("GETfxn must be either 'search/cell-line?' or 'cell-line/'")
#         }
# }