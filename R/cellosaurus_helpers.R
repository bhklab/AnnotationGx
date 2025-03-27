#' Create a query list for Cellosaurus database
#'
#' This function creates a query list for the Cellosaurus database based on the provided IDs and fields.
#' The query list is used to retrieve specific information from the database.
#'
#' @param ids A vector of IDs to be included in the query list.
#' @param from A character vector specifying the fields to be included in the query list.
#'             If the length of 'from' is 1, the same field will be used for all IDs.
#'             If the length of 'from' is equal to the length of 'ids', each ID will be paired with its corresponding field.
#'             Otherwise, an error will be thrown.
#' @param fuzzy A logical value indicating whether to perform a fuzzy search. Default is FALSE.
#'
#' @return A character vector representing the query list.
#'
#' @examples
#' AnnotationGx:::.create_cellosaurus_queries(c("ID1", "ID2", "ID3"), "Accession")
#' # Returns: "Accession:ID1" "Accession:ID2" "Accession:ID3"
#'
#' AnnotationGx:::.create_cellosaurus_queries(c("ID1", "ID2", "ID3"), c("Accession", "Name", "Species"))
#' # Returns: "Accession:ID1" "Name:ID2" "Species:ID3"
#'
#' @keywords internal
#' @noRd
.create_cellosaurus_queries <- function(ids, from, fuzzy = FALSE) {
  if (fuzzy) {
    ids <- paste0(cleanCharacterStrings(ids), "~")
  }

  # either from has to be one field, or the same length as ids
  if (length(from) == 1) {
    return(paste0(from, ":", ids))
  }
  if (length(from) != length(ids)) {
    stop("Length of 'from' must be 1 or the same length as 'ids'")
  }
  sapply(1:length(ids), function(i) {
    paste(from[i], ids[i], sep = ":")
  })
}



#' Build a Cellosaurus API request
#'
#' This function builds a Cellosaurus API request based on the provided parameters.
#'
#' @param query A character vector specifying the query terms for the Cellosaurus API.
#' @param to A character vector specifying the fields to include in the API response.
#' @param numResults An integer specifying the maximum number of results to return.
#' @param apiResource A character string specifying the API resource to query.
#' @param output A character string specifying the desired output format of the API response.
#' @param sort A character string specifying the field to sort the results by.
#' @param query_only A logical value indicating whether to return only the constructed URL without making the request.
#' @param ... Additional arguments to be passed to the function.
#'
#' @return A character string representing the constructed URL for the Cellosaurus API request.
#'
#' @examples
#' .build_cellosaurus_request(
#'   query = c("id:HeLa"), to = c("id", "ac", "hi", "ca", "sx", "ag", "di", "derived-from-site", "misspelling"),
#'   numResults = 1, apiResource = "search/cell-line", output = "TSV", sort = "ac",
#'   query_only = FALSE
#' )
#'
#' @keywords internal
#' @noRd
.build_cellosaurus_request <- function(
    query = c("id:HeLa"), to = c("id", "ac", "hi", "ca", "sx", "ag", "di", "derived-from-site", "misspelling"),
    numResults = 1, apiResource = "search/cell-line", output = "TSV", sort = "ac",
    query_only = FALSE, ...) {
  checkmate::assert_character(c(query, output))
  checkmate::assert_choice(apiResource, c("search/cell-line", "cell-line", "release-info"))
  checkmate::assert_choice(output, c("TSV", "TXT", "JSON", "XML"))

  base_url <- "https://api.cellosaurus.org"
  url <- httr2::url_parse(base_url)
  url$path <- .buildURL(url$path, apiResource)

  opts <- list()

  if (apiResource == "search/cell-line") {
    opts$q <- paste0(query, collapse = " ")
  } else if (apiResource == "cell-line") {
    url$path <- .buildURL(url$path, query)
  }

  # if the url$path has 2 // in the beginning, remove one
  if (startsWith(url$path, "//")) {
    url$path <- substr(url$path, 2, nchar(url$path))
  }

  if (!is.null(sort)) {
    opts$sort <- paste0(sort, " asc")
  }

  opts$fields <- paste0(to, collapse = ",")
  opts$format <- tolower(output)
  opts$rows <- numResults


  url$query <- opts
  url <- url |> httr2::url_build()
  if (query_only) {
    return(url)
  }
  url |> .build_request()
}



#' Get the Cellosaurus schema
#'
#' This function retrieves the Cellosaurus schema from the Cellosaurus API.
#' It internally calls the `.buildURL()`, `.build_request()`, `.perform_request()`,
#' and `.parse_resp_json()` functions to construct the API URL, send the request,
#' and parse the response.
#'
#' @return A list representing the Cellosaurus schema.
#'
#' @keywords internal
#' @noRd
.cellosaurus_schema <- function() {
  url <- .buildURL("https://api.cellosaurus.org/openapi.json")
  request <- .build_request(url)

  resp <- .perform_request(request)
  .parse_resp_json(resp)
}





#' Internal function to return the list of external resources available in Cellosaurus
#' @return A character vector of external resources available in Cellosaurus
#'
#' @keywords internal
#' @noRd
.cellosaurus_extResources <- function() {
  c(
    "4DN", "Abcam", "ABCD", "ABM", "AddexBio", "ArrayExpress",
    "ATCC", "BCGO", "BCRC", "BCRJ", "BEI_Resources",
    "BioGRID_ORCS_Cell_line", "BTO", "BioSample", "BioSamples",
    "cancercelllines", "CancerTools", "CBA", "CCLV", "CCRID",
    "CCTCC", "Cell_Biolabs", "Cell_Model_Passport", "CGH-DB",
    "ChEMBL-Cells", "ChEMBL-Targets", "CLDB", "CLO", "CLS",
    "ColonAtlas", "Coriell", "Cosmic", "Cosmic-CLP", "dbGAP",
    "dbMHC", "DepMap", "DGRC", "DiscoverX", "DSHB", "DSMZ",
    "DSMZCellDive", "EBiSC", "ECACC", "EFO", "EGA", "ENCODE",
    "ESTDAB", "FCDI", "FCS-free", "FlyBase_Cell_line", "GDSC",
    "GeneCopoeia", "GEO", "HipSci", "HIVReagentProgram", "Horizon_Discovery",
    "hPSCreg", "IARC_TP53", "IBRC", "ICLC", "ICLDB", "IGRhCellID",
    "IGSR", "IHW", "Imanis", "Innoprot", "IPD-IMGT/HLA", "ISCR",
    "IZSLER", "JCRB", "KCB", "KCLB", "Kerafast", "KYinno", "LiGeA",
    "LIMORE", "LINCS_HMS", "LINCS_LDP", "Lonza", "MCCL", "MeSH",
    "MetaboLights", "Millipore", "MMRRC", "NCBI_Iran", "NCI-DTP", "NHCDR",
    "NIHhESC", "NISES", "NRFC", "PerkinElmer", "PharmacoDB", "PRIDE",
    "Progenetix", "PubChem_Cell_line", "RCB", "Rockland", "RSCB", "SKIP",
    "SKY/M-FISH/CGH", "SLKBase", "TKG", "TNGB", "TOKU-E", "Ubigene",
    "WiCell", "Wikidata", "Ximbio"
  )
}
