#' Maps cell line names to accession numbers
#'
#' This function takes a vector of cell line names and maps them to accession numbers
#' using the Cellosaurus database. It performs a parallel request to retrieve the
#' mapping information and returns a data table with the results.
#'
#' @param ids A character vector of cell line names.
#' @param numResults The number of results to return for each query. Default is 1.
#' @param from The field to query from. Default is "id".
#' @param to The field to query to. Default is both "id" and "ac".
#' @param query_only If TRUE, returns the query URL instead of the results. Default is FALSE.
#' @param raw If TRUE, returns the raw response instead of a data table. Default is FALSE.
#' @param ... Additional arguments to pass to the request.
#'
#' @return A data table with the mapping information, including the cell line name,
#' accession number, and Cellosaurus ID.
#'
#' @examples
#' mapCell2Accession(c("A549", "HeLa"))
#' @export
mapCell2Accession <- function(
    ids,  numResults = 1, from = "id", to= c("id", "ac"),
    query_only = FALSE, raw = FALSE, ...
    ){
    if(!is.character(ids)) {
        .warn("Input names are not character, coercing to character")
        ids <- as.character(ids)
    }

    # create query list
    queries <- .create_query_list(ids, from)
    names(queries) <- ids

    requests <- lapply(queries, function(q){
        .build_cellosaurus_request(
        query = q, to = to,
        output = "TSV", numResults = numResults,...)
    })
    if(query_only) return(lapply(requests, function(req) req$url))

    responses <- .perform_request_parallel(requests)
    if(raw) return(responses)
    names(responses) <- as.character(ids) # in case its an numeric ID  like cosmic ids
    lapply(ids, function(name){
        resp <- responses[[name]]
        resp <- readr::read_tsv(resp$body, skip = 14, show_col_types = FALSE)
        # if tibble has no rows, add a row of NAs
        if(nrow(resp)==0) resp <- tibble::tibble(ac = NA, id = NA, query = name)
        else {
            resp[[paste0("query:",from)]] <- name
            resp$query <- queries[[name]]
        }
        # add name to the response tibble
        resp |> .asDT()
    })|> data.table::rbindlist(use.names=TRUE)

}

