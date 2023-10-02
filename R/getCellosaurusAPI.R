
#' Create a list of query URLS for Cellosaurus API
#'
#' @description
#' This function creates a queryURL for the cellosaurus API using a list of cell line names
#'
#' @details 
#' Function to create a URL query for Cellosaurus to search for a cell-line using its name 
#' An example call:   computedURLs <- createQueryURLs(api = "https://api.cellosaurus.org/", cl_names = ["22rv1", "Hela"], fields = c("id", "ac"))
#' @return A list of URLS
#' @param api is the link to the API to build the URL. i.e "https://api.cellosaurus.org/"
#' @param cl_names is a list of the cell line names
#' @param format is the type of format to return from the API. Can be "txt" or "json" 
#' @param num_results is the number of of items to return, DEFAULT=1
#' @param GETfxn is the function to use on the cellosaurus website. Currently only supports "search/cell-line?"
#' @param fields is a list of desired fields to include in the response 
#' 
#' @md
#' 
createQueryURLs <- 
    function(api,
             cl_names,
             format = "txt",
             num_results = 1,
             GETfxn = "search/cell-line?",
             fields) {
        # create urls 
        computedURLs <- paste0(api,
                            GETfxn,
                            "q=",
                            "idsy:",
                            gsub(" ", "%20",cl_names),
                            "&",
                            "rows=",
                            num_results,
                            "&",
                            "format=",format,
                            "&",
                            "fields=", paste(fields, collapse=",")
                            )
        return(computedURLs)
}

#' Query Cellosaurus
#' 
#' @description
#' This function takes a list of cell line names and interested fields and gets responses from the Cellosaurus API 
#'
#' @details 
#' Function to get responses from Cellosaurus API
#' 
#' @return A list of responses
#' @param cl_names is a list of the cell line names
#' @param fields is a list of desired fields to obtain for each cell line in the API query, i.e if only trying to get synonynms and primary accesssion then fields=c("sy", "ac"). see https://api.cellosaurus.org/static/fields_help.html for all fields.
#'  
#' @md
#' @export
#' 
getCellosaurusAPI <-
    function(
            cl_names,                       # List of cell line names 
            fields = c("id",               # Recommended name. Most frequently the name of the cell line as provided in the original publication.
                            "ac",           # Primary accession. It is the unique identifier of the cell line. It is normally stable across Cellosaurus versions ...
                            "sy",           # List of synonyms.
                            "misspelling",  # Identified misspelling(s) of the cell line name
                            "din",          # Disease(s) suffered by the individual from which the cell line originated with its NCI Thesaurus or ORDO identifier.
                            "ca",           # Category to which a cell line belongs, one of 14 defined terms. Example: cancer cell line, hybridoma, transformed cell line.
                            "sx",           # Sex
                            "ag",           # Age at sampling time of the individual from which the cell line was established.
                            "sampling-site", # Body part, organ, cell-type the cell line is derived from
                            "metastatic-site" # Body part, organ the cell line is derived from in the case of a cell line originating from a cancer metastasis.
                            # "cc"            # comments
                )
            ){
        cellosaurus_api_url <- "https://api.cellosaurus.org/"
        
        computedURLs <- createQueryURLs(api = cellosaurus_api_url, cl_names = cl_names, fields = fields)
        responseList <- BiocParallel::bplapply(computedURLs, function(x) GET(x)) 
        names(responseList) <- cl_names
        return(responseList)
}

#' Clean cellosaurus responses
#'
#' @description
#' This function takes a list of Cellosaurus Responses and cleans them for use 
#'
#' @details This function takes a list of Cellosaurus Responses and cleans them for use 
#' @return A list of responses
#' @param responseList is a list of responses
#' 
#' @md
#' @export
#' 
cleanCellosaurusResponse <- 
    function(responseList){
        # Get content of each response, then separate content on newline character
        responseContent <- lapply(lapply(responseList, httr::content),
                                    function(x) strsplit(x=x, split="\n"))
        
        #Remove first 15 rows of content
        responseContent_sub <- lapply(responseContent, function(x) x[[1]][-(1:15)]) 

        # Split on first "  " appearance
        responseContent_sub_split <- lapply(responseContent_sub, function(x) strsplit(x, split = "  ."))
        
        # rbind responses (do.call returns as one large matrix instead of dataframe)
        df_ <- lapply(responseContent_sub_split, function(x) do.call(rbind, x))

        # convert each response from matrix to data.table
        df_2 <- lapply(df_, function(x) data.table(x))

        # rbinds all responses (creates new column so all of cell line x will have its name in col cellLine)
        df_3<- rbindlist(df_2, idcol = "cellLine")
        df_3 <- df_3[V1!="//"]

        # Collapse all rows with the same cellLine & V1 (most often rows for cc) and separate by "; "
        df_4 <- df_3[, list(data = paste0(unique(na.omit(V2)), collapse ="; ")), by = c("cellLine", "V1")]

        # transpose 
        df_5 <- dcast(df_4, cellLine ~ ...)

        return(df_5) 
}