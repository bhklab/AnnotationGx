#' Build a UniChem query URL
#'
#' This function builds a UniChem query URL based on the specified endpoint.
#'
#' @param endpoint The UniChem endpoint to query (valid options: "compounds", "connectivity", "images", "sources")
#' @param query_only Logical indicating whether to return only the query URL without building it (default: FALSE)
#'
#' @return `httr2::httr2_url` object if `query_only` is TRUE, otherwise the built URL.
#'
#' @examples
#' .build_unichem_query("sources")
#' .build_unichem_query("connectivity", query_only = TRUE)
#' 
#' @noRd
#' @keywords internal
.build_unichem_query <- function(
    endpoint, query_only = FALSE
) {

    valid_endpoints <- c("compounds", "connectivity", "images", "sources")
    checkmate::assert_subset(endpoint, valid_endpoints)

    unichem_api <- "https://www.ebi.ac.uk/unichem/api/v1"
    url <- httr2::url_parse(unichem_api)
    url$path <- .buildURL(url$path, endpoint)

    if (query_only) return(url)

    return(httr2::url_build(url))
}


#' Build a UniChem compound request
#'
#' This function builds a UniChem compound request based on the provided parameters.
#'
#' @param type The type of compound identifier to search for. Valid types are "uci", "inchi", "inchikey", and "sourceID".
#' @param compound The compound identifier to search for.
#' @param sourceID The source ID to search for if the type is "sourceID". Defaults to NULL.
#' @param ... Additional arguments.
#'
#' @return A `httr2_request`  request object for the UniChem compound query.
#'
#' @examples
#' .build_unichem_compound_req(type = "uci", compound = "538323")
#' .build_unichem_compound_req(type = "sourceID", sourceID = 22, compound = "2244")
#' 
#' @noRd
#' @keywords internal
.build_unichem_compound_req <- function(
    type, compound, sourceID = NULL, ...
){
    valid_types <- c("uci", "inchi", "inchikey", "sourceID")
    checkmate::assert_subset(type, valid_types)

    base_url <- .build_unichem_query("compounds")

    body <- list(
        type = type,
        compound = compound
    )

    body$sourceID <- if (type == "sourceID") {
        checkmate::assert_integerish(
            x = sourceID,
            lower = 1,
            upper = max(getUnichemSources()$SourceID),
            len = 1
            )
        sourceID
    } else NULL


    request <- base_url |> 
        .build_request() |>
        httr2::req_body_json(body) 

    return(request)

}
