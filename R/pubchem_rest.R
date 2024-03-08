#' Retrieve PubChem compound information
#'
#' This function retrieves compound information from PubChem using the PubChem REST API.
#' Used by other functions to retrieve compound information.
#'
#' @param ids A vector of compound identifiers.
#' @param from The source namespace of the compound identifiers. Default is 'cid'.
#' @param to The target namespace for the compound information. Default is 'property'.
#' @param properties A character vector specifying the properties to retrieve.
#' @param raw Logical indicating whether to return the raw query results. Default is FALSE.
#' @param query_only Logical indicating whether to only perform the query without retrieving the results. Default is FALSE.
#' @param output The format of the query results. Default is 'JSON'.
#' @param ... Additional arguments to be passed to the query_pubchem_rest function.
#'
#' @return A data.table containing the retrieved compound information.
#'
#' @examples
#' properties <- c("Title", "MolecularFormula", "InChIKey", "CanonicalSMILES")
#' getPubchemCompound(c(3672, 176870), from = "cid", to = "property", properties = properties)
#'
#' @export
getPubchemCompound <- function(
    ids, from = "cid", to = "property", properties = c("Title", "InChIKey"),
    raw = FALSE, query_only = FALSE, output = "JSON", ...) {
  funContext <- .funContext("getPubchemCompound")


  to_ <- if (to == "property") {
    checkmate::assert_atomic(properties, all.missing = FALSE)
    checkmate::assert_character(properties)
    to <- paste0(to, "/", paste0(properties, collapse = ","))
  } else {
    to
  }

  requests <- lapply(ids, function(x) {
    .build_pubchem_rest_query(
      id = x, domain = "compound", namespace = from, operation = to_, output = output,
      raw = raw, query_only = query_only, ...
    )
  })
  if (query_only) {
    return(requests)
  }

  resps_raw <- httr2::req_perform_sequential(requests, on_error = "continue")
  .debug(funContext, " Number of responses: ", length(resps_raw))
  names(resps_raw) <- ids
  if (raw) {
    return(resps_raw)
  }


  # Parse the responses
  resps <- .parse_pubchem_rest_responses(resps_raw)
  failed <- sapply(resps_raw, httr2::resp_is_error, USE.NAMES = T)

  if (any(failed)) {
    .warn(funContext, " Some queries failed. See the 'failed' object for details.")
    failures <- lapply(resps_raw[failed], function(resp) {
      .parse_resp_json(resp)$Fault
    })
  } else {
    failures <- NULL
  }

  if (from != "name") {
    responses <- data.table::rbindlist(resps, fill = TRUE)
  } else {
    responses <- data.table::rbindlist(resps, idcol = from, fill = TRUE)
  }
  data.table::setnames(responses, "V1", to, skip_absent = TRUE)

  attributes(responses)$failed <- failures

  responses
}


#' Map compound names to PubChem CIDs
#'
#' This function maps compound names to PubChem CIDs using the PubChem REST API.
#'
#' @param names A character vector of compound names.
#' @param first Logical indicating whether to return only the first CID for each compound name (default is FALSE).
#' @param ... Additional arguments to be passed to the getPubchemCompound function.
#'
#' @return A character vector of PubChem CIDs.
#'
#' @examples
#' mapCompound2CID(c("aspirin", "caffeine"))
#'
#' @export
mapCompound2CID <- function(
    names, first = FALSE, ...) {
  result <- getPubchemCompound(
    ids = names, from = "name", to = "cids", ...
  )

  if (first) {
    return(result[!duplicated(result$name), ])
  } else {
    return(result)
  }
}


#' Map PubChem Compound IDs to Properties
#'
#' This function maps PubChem Compound IDs to specified properties using the PubChem REST API.
#' See `getPubchemProperties` for a list of available properties.
#'
#' @param ids A vector of PubChem Compound IDs.
#' @param properties A vector of property names to retrieve for each compound.
#' @param ... Additional arguments to be passed to the `getPubchemCompound` function.
#'
#' @return A data frame containing the mapped properties for each compound.
#'
#' @examples
#' mapCID2Properties(ids = c(123, 456), properties = c("MolecularWeight", "CanonicalSMILES"))
#'
#' @export
mapCID2Properties <- function(
    ids, properties, ...) {
  getPubchemCompound(
    ids = ids, from = "cid", to = "property", properties = properties, ...
  )
}

#' Retrieves the PubChem XML schema and extracts property information.
#'
#' This function retrieves the PubChem XML schema from the specified URL and
#' extracts the property information from it. The property information includes
#' the name and type of each property.
#'
#' @return A data table containing the extracted property information.
#'
#' @export
getPubchemProperties <- function() {
  url <- "https://pubchem.ncbi.nlm.nih.gov/pug_rest/pug_rest.xsd"
  response <- .build_request(url) |>
    .perform_request()

  node_list <- xml2::read_xml(response$body) |>
    xml2::xml_children() |>
    xml2::as_list()

  properties <- node_list[[3]]$complexType$sequence$element$complexType$sequence

  lapply(properties, function(x) {
    list(
      name = attr(x, "name"),
      type = gsub("xs:", "", attr(x, "type"))
    ) |> .asDT()
  }) |> data.table::rbindlist()
}
