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
#' @param BPPARAM A BiocParallel parameter object controlling the parallelization.
#' @param prioritizeParent If TRUE, prioritizes the parent cell line when multiple matches are found. Default is FALSE.
#'                        When prioritizeParent is TRUE, if multiple matches are found for a cell line name, the function
#'                        will prioritize the parent cell lines over other matches. This can be useful when dealing with
#'                        cell line hierarchies where the parent cell line represents a broader category.
#'                        An example of this is trying to map using id "BT474" which returns "CVCL_YX79" which corresponds
#'                        to "BT474 A3" whereas "BT-474" exists in the database as "CVCL_0179". If prioritizeParent is TRUE,
#'                        the function will prioritize "CVCL_0179" over "CVCL_YX79" since "BT-474" is the parent cell line of 
#'                        "BT474 A3".
#' @param orderby The field to order the results by. Default is "ac" to order by accession number. 
#' @param ... Additional arguments to pass to the request.
#'
#' @return Depending on parameters, either a:
#' `data.table` with the "id", "ac", "query", and "query:id" columns.
#' `list` of query URLs if `query_only` is TRUE.
#' `list` of raw responses if `raw` is TRUE.
#'
#' @examples
#' mapCell2Accession(c("A549", "HeLa"))
#' @export
mapCell2Accession <- function(
    ids, numResults = 1000, from = "id", to = c("id", "ac"),
    prioritizeParent = FALSE, orderby = "ac",
    query_only = FALSE, raw = FALSE, BPPARAM = BiocParallel::SerialParam(), ...
) {
    
    # Input validation and coercion
    if(!is.character(ids)) {
        .warn("Input names are not character, coercing to character")
        ids <- as.character(ids)
    }

    if(prioritizeParent) to <- c(to, "hi")

    # create query list
    queries <- .create_query_list(ids, from)
    names(queries) <- ids

    requests <- .bplapply(queries, function(query){
        .build_cellosaurus_request(
            query = query, 
            to = to,
            output = "TSV", 
            numResults = {
                if(prioritizeParent) 1000
                else numResults
            },
            ...
        )
    }, BPPARAM = BPPARAM
    )
    if(query_only) return(lapply(requests, function(req) req$url))

    responses <- .perform_request_parallel(requests)
    if(raw) return(responses)


    names(responses) <- as.character(ids) # in case its an numeric ID  like cosmic ids
    responses_dt <- lapply(ids, function(name){
        resp <- responses[[name]]
        resp <- readr::read_tsv(resp$body, skip = 14, show_col_types = FALSE)
        # if tibble has no rows, add a row of NAs
        if(nrow(resp)==0) {
            resp <- tibble::tibble(ac = NA, id = NA, query = queries[[name]])
        }
        else {
            resp$query <- queries[[name]]
        }
        resp[[paste0("query:",from)]] <- name
        # add name to the response tibble
        resp |> .asDT()
    })|> data.table::rbindlist(use.names=TRUE, fill=TRUE)

    if(!prioritizeParent) return(responses_dt)
    if(all(is.na(responses_dt$hi))) return(responses_dt)

    return(.prioritize_parent(responses_dt))
}


.prioritize_parent <- function(responses_dt) {
    responses_dt[, c("parentAC", "parentID") := tstrsplit(hi, "!", fixed = TRUE)]
    responses_dt <- responses_dt[, -"hi"]

    if(all(is.na(responses_dt$parentAC))) return(responses_dt[, -c("parentAC", "parentID")])

    parentACs <- na.omit(unique(responses_dt$parentAC))

}
