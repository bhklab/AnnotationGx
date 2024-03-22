#' Annotate Cell Accession
#'
#' This function takes a Cellosaurus accession and returns annotations for the cell line.
#'
#' @param accessions The Cellosaurus accession to annotate.
#' @param to A character vector specifying the types of annotations to retrieve. Possible values include "id", "ac", "hi", "sy", "ca", "sx", "ag", "di", "derived-from-site", "misspelling", and "dt".
#'
#' @return A data frame containing the annotations for the cell line.
#'
#' @examples
#' annotateCellAccession("CVCL_0031")
#' annotateCellAccession("CVCL_0031", to = c("id", "ac", "hi", "sy"))
#'
#' @export
annotateCellAccession <- function(
    accessions,
    to = c("id", "ac", "hi", "sy", "ca", "sx", "ag", "di", "derived-from-site", "misspelling", "dt")
    )
{

    requests <- parallel::mclapply(accessions, function(accession) {
        .build_cellosaurus_request(
            query = accession,
            to = to,
            numResults = 1,
            apiResource = "search/cell-line",
            output = "TXT",
            sort = NULL,
            query_only = FALSE
        )
    })
        
    responses <- .perform_request_parallel(requests)
    names(responses) <- accessions
    responses_dt <- parallel::mclapply(accessions,function(name) {
        resp <- responses[[name]]
        .parse_cellosaurus_lines(resp) |> 
            unlist(recursive = FALSE) |> 
            .processEntry() |>
            .formatSynonyms()
    }
    ) |> data.table::rbindlist(fill = TRUE)

    responses_dt

}

