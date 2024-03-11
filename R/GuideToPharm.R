#' Get data from the Guide to PHARMACOLOGY Database Web Services
#'
#' @param ids `character()` or `integer()` Identifiers to query the web
#'   service with. If excluded, the entire record for the specified service
#'   is returned.
#' @param service `character(1)` Which Guide to PHARMACOLOGY web service
#'   to query. Defaults to 'ligands'. Other options are 'targets', 'interactions',
#'   'diseases' and 'references'.
#' @param id_type `character(1)` What type of identifiers are in `ids`? Defaults
#'   to 'name', for drug name. Other options are 'accession', which accepts
#'   PubChem CIDs.
#' @param ... Force subsequent parameters to be named. Not used.
#'
#' @return A `data.table` of query results.
#'
#' @details
#' The API reference documentation can be found here:
#' https://www.guidetopharmacology.org/webServices.jsp
#'
#' There is also a Python interface available for querying this API. See:
#' https://github.com/samirelanduk/pygtop
#'
#' @importFrom data.table data.table as.data.table rbindlist setnames
#' @importFrom jsonlite fromJSON
#' @importFrom httr RETRY GET status_code
#'
#' @export
# getGuideToPharm <- function(
#     ids = character(),
#     service = c("ligands", "targets", "interactions", "diseases", "references"),
#     id_type = c("name", "accession"),
#     ...,
# ){


#     checkmate::assert_atomic(ids, any.missing = FALSE, min.len = 1)
#     checkmate::assert_character(service, len = 1)
#     checkmate::assert_character(id_type, len = 1)

#     url <- httr2::url_parse("https://www.guidetopharmacology.org/services")
#     url$path <- .buildURL(url$path, service)

#     opts <- list()

#     opts[id_type] <- paste0(ids, collapse = ",")

# }

