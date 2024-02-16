#' Maps cell line names to accession numbers
#'
#' This function takes a vector of cell line names and maps them to accession numbers
#' using the Cellosaurus database. It performs a parallel request to retrieve the
#' mapping information and returns a data table with the results.
#'
#' @param ids A vector of cell line names to be mapped.
#' @param numResults The maximum number of results to retrieve for each cell line name.
#' @param from The field to search for the cell line name in the Cellosaurus database.
#' @param to The field to map to, default is both cell line name (id) and accession number (ac).
#' @param ... Additional arguments to be passed to the Cellosaurus API.
#'
#' @return A data table with the mapping information, including the cell line name,
#' accession number, and Cellosaurus ID.
#'
#' @examples
#' mapCell2Accession(c("A549", "HeLa"))
#' @export
mapCell2Accession <- function(
    ids,  numResults = 1, from = "id", to= c("id", "ac"), ...
    ){
    if(!is.character(ids)) {
        .warn("Input names are not character, coercing to character")
        ids <- as.character(ids)
    }
    requests <- lapply(ids, function(name){
        .build_cellosaurus_request(
        from = from, to = to,
        query = name, output = "TSV", numResults = numResults,...)
    })


    responses <- .perform_request_parallel(requests)
    names(responses) <- as.character(ids) # in case its an numeric ID  like cosmic ids
    lapply(ids, function(name){
        resp <- responses[[name]]
        resp <- readr::read_tsv(resp$body, skip = 14, show_col_types = FALSE)
        # if tibble has no rows, add a row of NAs
        if(nrow(resp)==0) resp <- tibble::tibble(ac = NA, id = NA, query = name)
        else resp$query <- name
        # add name to the response tibble
        resp |> .asDT()
    })|> data.table::rbindlist(use.names=TRUE)

}



.build_cellosaurus_request <- function(
    from= "id", query="Hela",
    to = c("id", "ac", "ca", "sx", "ag", "di", "derived-from-site",  "misspelling"),
    numResults = 1, extResource = NULL, apiResource= "search/cell-line", output = "TSV"
){
    checkmate::assert_character(c(from, query, output))
    checkmate::assert_subset(to, .cellosaurus_fields())
    checkmate::assert_choice(apiResource, c("search/cell-line", "cell-line", "release-info"))
    checkmate::assert_choice(output, c("TSV", "TXT", "JSON", "XML"))

    opts <- list()
    opts$q <- {
        if (from == "dr") paste0("dr:", extResource, ";", query)
        else  paste0(from, ":", query)
    }
    opts$fields <- paste(to, collapse = ",")
    opts$format <- tolower(output)
    opts$rows <- numResults


    base_url <- "https://api.cellosaurus.org"
    url <- httr2::url_parse(base_url)
    url$path <- .buildURL(url$path, apiResource)
    url$query <- opts
    url |> httr2::url_build() |> .build_request()
}