#' Annotate Cell Accession
#'
#' This function takes a Cellosaurus accession and returns annotations for the cell line.
#'
#' @param cellosaurus_accession The Cellosaurus accession to annotate.
#' @param to A character vector specifying the types of annotations to retrieve. Possible values include "id", "ac", "hi", "sy", "ca", "sx", "ag", "di", "derived-from-site", "misspelling", and "dt".
#'
#' @return A data frame containing the annotations for the cell line.
#'
#' @examples
#' annotateCellACcession("CVCL_0031", to = c("id", "ac", "hi", "sy"))
#'
#' @export
annotateCellACcession <- function(
    cellosaurus_accession,
    to = c("id", "ac", "hi", "sy", "ca", "sx", "ag", "di", "derived-from-site", "misspelling", "dt")
    )
{

    resp <- .build_cellosaurus_request(
            query = cellosaurus_accession,
            to = to,
            numResults = 1,
            apiResource = "search/cell-line",
            output = "TXT",
            sort = NULL,
            query_only = FALSE
        )  |> .perform_request() 
    x  <- .parse_cellosaurus_lines(resp)[[1]]
    dt <- .processEntry(x) |>
        .formatSynonyms()
    dt

}

