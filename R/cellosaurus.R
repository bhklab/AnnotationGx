#' Maps cell line names to accession numbers
#'
#' This function takes a vector of cell line names and maps them to accession numbers
#' using the Cellosaurus database. It performs a parallel request to retrieve the
#' mapping information and returns a data table with the results.
#'
#' @param names A vector of cell line names to be mapped.
#' @param numResults The maximum number of results to retrieve for each cell line name.
#' @param from The field to search for the cell line name in the Cellosaurus database.
#' @param ... Additional arguments to be passed to the Cellosaurus API.
#'
#' @return A data table with the mapping information, including the cell line name,
#' accession number, and Cellosaurus ID.
#'
#' @examples
#' mapCellline2Accession(c("A549", "HeLa"))
#' @export
mapCell2Accession <- function(
    names,  numResults = 1, from = "id", ...
    ){

    requests <- lapply(names, function(name){
        .build_cellosaurus_request(
        from = from, to = c("id", "ac"),
        query = name, output = "TSV", numResults = numResults,...)
    })


    responses <- .perform_request_parallel(requests)
    names(responses) <- names
    lapply(names, function(name){
        resp <- responses[[name]]
        resp <- readr::read_tsv(resp$body, skip = 14, show_col_types = FALSE)
        # if tibble has no rows, add a row of NAs
        if(nrow(resp)==0) resp <- tibble::tibble(ac = NA, id = NA, query = name)
        else resp$query <- name
        # add name to the response tibble
        resp |> .asDT()
    })|> data.table::rbindlist(use.names=TRUE)

}