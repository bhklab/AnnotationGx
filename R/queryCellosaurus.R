## =========================================
## Parse Cellosaurus xml
## -----------------------------------------

#' Access and parse Cellosaurus xml for developers
#'
#' @description
#' This function reads Cellosaurus XML and parses parent node for each cell line, returning an XML document.
#' This enables parsing any child node of interest.
#'
#' @details This function reads Cellosaurus XML and parses parent node for each cell line, returning an XML document.
#'
#' @return An `XML` document of Cellosaurus
#' @references
#' Bairoch A.The Cellosaurus, a cell line knowledge resource.J. Biomol. Tech. 29:25-38(2018) DOI: 10.7171/jbt.18-2902-002; PMID: 29805321; PMCID: PMC5945021
#'
#' @param url is cellosaurus link to xml. This should be a valid link to Cellosaurus xml. Default is `https://ftp.expasy.org/databases/cellosaurus/cellosaurus.xml`.
#' @param cellline_input is any cell line identifier. Cell line name(s) or `Cellosaurus ID (CVCL ID)`` can be provided as the input.
#' @param namespace is either cell line name (default) or Cellosaurus ID (CVCL ID).
#' @param verbose is TRUE by default
#'
#' @md
#' @importFrom xml2 read_xml xml_find_all xml_find_first xml_text xml_add_child
#' @export
queryCellosaurus <- function(url = "https://ftp.expasy.org/databases/cellosaurus/cellosaurus.xml",
        cellline_input, namespace = "name", verbose = TRUE) {
      if (namespace != "name" & namespace != "cvclid") {
        if (verbose) {
          message("invalid input. Please provide a valid namespace : 'name' or 'cvclid'")
        }
        return(NULL)
      }
      if (verbose) {
        message(paste(
          "xml read started from",
          url,
          format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        ))
      }
        main_xml <- read_xml(url)
        if (verbose) {
          message(paste(
            "xml read completed at",
            format(Sys.time(), "%Y-%m-%d %H:%M:%S")
          ))
        }
        root_node <- xml_new_root("cell-line-list")
        cellline_input <- cellline_input[!is.na(cellline_input)]
        if (namespace == "name") {
          for (ip in 1:length(cellline_input)) {
            xmlObject <-
              xml_find_first(
                main_xml,
                paste(
                  "//cell-line/name-list/name[normalize-space(text()) = '",
                  cellline_input[ip],
                  "']/../..",
                  sep = ""
                )
              )
            if (length(xmlObject) > 0) {
          xml_add_child(root_node, xmlObject)
        }
      }
    }
    else if (namespace == "cvclid") {
      for (ip in 1:length(cellline_input)) {
        xmlObject <-
          xml_find_first(
            main_xml,
            paste(
              "//cell-line/accession-list/accession[@type = 'primary'][normalize-space(text()) = '",
              cellline_input[ip],
              "']/../..",
              sep = ""
            )
          )
        if (length(xmlObject) > 0) {
          xml_add_child(root_node, xmlObject)
          }
        }
    }
    if   (verbose) {
        message(paste(
        "completed fetching nodesets for",
        length(cellline_input),
        "cell line(s)"
      ))
    }
    return(root_node)
    }

## ============================= CELLOSAURUS WORK ==============================
# these functions will be put into annotationGx for ease:

#' Create a list of query URLS for Cellosaurus API
#'
#' @description
#' This function creates a queryURL for the cellosaurus API using a list of cell line names
#'
#' @details 
#' Function to create a URL query for Cellosaurus to search for a cell-line using its name 
#' An example call:   finalURLs <- createQueryURLs(api = CLurl, cl_names = cl_names, fields = c("id", "ac"))
#' @return A list of URLS
#' @param api is the link to the API to build the URL. i.e "https://api.cellosaurus.org/"
#' @param cl_names is a list of the cell line names
#' @param format is the type of format to return from the API. Can be "txt" or "json" 
#' @param clnum is the number of of cell lines to return, DEFAULT=1
#' @param GETfxn is the function to use on the cellosaurus website. Currently only supports "search/cell-line?"
#' @param fields is a list of desired fields to include in the response 
#' 
#' @md
#' 
createQueryURLs <- 
    function(api,
             cl_names,
             format = "txt",
             clnum = 1,
             GETfxn = "search/cell-line?",
             fields) {
        # create urls 
        finalURLs <- paste0(api,
                            GETfxn,
                            "q=",
                            "idsy:",
                            gsub(" ", "%20",cl_names),
                            "&",
                            "rows=",
                            clnum,
                            "&",
                            "format=",format,
                            "&",
                            "fields=", paste(fields, collapse=",")
                            )
        return(finalURLs)
}

#' Query Cellosaurus
#'
#' @description
#' This function takes a list of cell line names and gets responses from the Cellosaurus API 
#'
#' @details 
#' Function to get responses from Cellosaurus API
#' 
#' @return A list of responses
#' @param cl_names is a list of the cell line names
#' 
#' @md
#' @export
#' 
getCellApi <-
    function(cl_names){
        CLurl <- "https://api.cellosaurus.org/"

        fields <- c("id",           # Recommended name. Most frequently the name of the cell line as provided in the original publication.
                    "ac",           # Primary accession. It is the unique identifier of the cell line. It is normally stable across Cellosaurus versions ...
                    "sy",           # List of synonyms.
                    "misspelling",  # Identified misspelling(s) of the cell line name
                    "din",           # Disease(s) suffered by the individual from which the cell line originated with its NCI Thesaurus or ORDO identifier.
                    "ca",           # Category to which a cell line belongs, one of 14 defined terms. Example: cancer cell line, hybridoma, transformed cell line.
                    "sx",           # Sex
                    "ag",           # Age at sampling time of the individual from which the cell line was established.
                    "sampling-site", # Body part, organ, cell-type the cell line is derived from
                    "metastatic-site" # Body part, organ the cell line is derived from in the case of a cell line originating from a cancer metastasis.
                    # "cc"            # comments
        )
        
        finalURLs <- createQueryURLs(api = CLurl, cl_names = cl_names, fields = fields)
        responseList <- bplapply(finalURLs, function(x) GET(x)) 
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