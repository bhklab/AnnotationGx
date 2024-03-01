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
    # This block of code prioritizes parent records from the responses
    # and is meant to deal with the issue of cell line names that may not be 
    # exactly the same as the parent cell line names in the Cellosaurus database.
    # i.e using BT474 would return 'CVCL_YX79' which corresponds to 'BT474 A3'
    #       whereas BT-474 exists in the database as 'CVCL_0179'
    # If prioritizeParent is FALSE, it returns the original data table.
    # If prioritizeParent is TRUE, it performs the following steps:
    # 1. Split the "hi" column into "parentAC" and "parentID" columns using "!" as the delimiter.
    # 2. Remove the "hi" column.
    # 3. Check if all the values in the "parentAC" column are NA. If so, return the data table 
    #       with "parentAC" and "parentID" columns removed.
    # 4. Get the unique values in the "parentAC" column.
    # 5. Check if all the values in the "ac" column are present in the unique values of the "parentAC" column.
    #    - If true, move all the rows that are parents to the top of the data table.
    #    - If false, add the parentAC and parentID pairs to the top of the data table.
    # 6. Remove the "parentAC" and "parentID" columns from the final data table and return it.
    if(!prioritizeParent) return(responses_dt)
    if(all(is.na(responses_dt$hi))) return(responses_dt)
    
    responses_dt[, c("parentAC", "parentID") := data.table::tstrsplit(responses_dt$hi, " ! ", fixed = TRUE)]
    responses_dt[, c('hi') := NULL]
    

    # make sure all the unique parentAC values are also in the "ac" column
    parentACs <- na.omit(unique(responses_dt$parentAC))
    responses_dt <- 
        if(all(parentACs %in% responses_dt$ac)) {
            # if so, move all the rows that are parents to the top
            parentRows <- responses_dt$ac %in% parentACs

            parentDT <- responses_dt[parentRows, ]
            childDT <- responses_dt[!parentRows, ]
            rbind(parentDT, childDT)

        } else{
            # add the parentAC and parentID pairs to the top of the table

            new_rows <- unique(
                responses_dt[parentAC %in% parentACs[!parentACs %in% responses_dt$ac], .(ac = parentAC, id = parentID, query = query, `query:id` = `query:id`)]
            )
            parent_rows <- responses_dt[parentAC %in% parentACs,]
            child_rows <- responses_dt[!parentAC %in% parentACs,]
            new_dt <- data.table::rbindlist(list(parent_rows, new_rows, child_rows), use.names=TRUE, fill=TRUE)
            new_dt[]
        }
    # groupby query and query:id 
    # for each group, sort by the highest number of parentAC counts
    if(orderby == "ac") data.table::setorderv(responses_dt, c("query","ac"))
    responses_dt[, c("parentAC", "parentID") := NULL]

    return(responses_dt[1:numResults])
}


