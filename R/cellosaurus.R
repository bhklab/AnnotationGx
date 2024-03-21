fields <- fields <- c("AC", "CA", "DT", "ID", "DI", "DR", "HI", "OI", "OX", "AG", "SX", "SY") |> tolower()

#' Map Cell Line IDs to Accession Numbers
#'
#' This function maps cell line IDs to accession numbers using the Cellosaurus database.
#'
#' @param ids A character vector of cell line IDs.
#' @param numResults The maximum number of results to return for each query. Default is 1000.
#' @param from The type of input IDs. Possible values are "idsy" (default), "ac", "id", "sy", and "misspelling".
#' @param sort The sorting order of the results. Possible values are "ac" (default), "id", "sy", and "misspelling".
#' @param keep_duplicates Logical indicating whether to keep duplicate results. Default is FALSE.
#' @param query_only Logical indicating whether to return only the query URLs. Default is FALSE.
#' @param raw Logical indicating whether to return the raw HTTP responses. Default is FALSE.
#' @param parsed Logical indicating whether to parse the response text. Default is FALSE.
#' @param ... Additional arguments to be passed to the underlying functions.
#'
#' @return A data.table containing the mapped cell line IDs and accession numbers.
#'
#' @examples
#' mapCell2Accession(ids = c("A549", "MCF7"))
#'
#' @export
mapCell2Accession <- function(
    ids, numResults = 1000, from = "idsy", sort = "ac", keep_duplicates = FALSE, 
    query_only = FALSE, raw = FALSE, parsed = FALSE, ...) {

  # Input validation and coercion
  if (!is.character(ids)) {
    .warn("Input names are not character, coercing to character")
    ids <- as.character(ids)
  }
  to = c("ac", "id", "sy", "misspelling")

  # create query list
  queries <- .create_cellosaurus_queries(ids, from)
  names(queries) <- ids

  # build the list of requests
  requests <- parallel::mclapply(queries, function(query) {
    .build_cellosaurus_request(
      query = query,
      to = to,
      numResults = numResults,
      sort = sort,
      output = "TXT",
      ...
    )
  })

  if (query_only) return(lapply(requests, function(req) req$url))
  
  # perform the requests
  responses <- .perform_request_parallel(requests)
  names(responses) <- as.character(ids) # in case its an numeric ID  like cosmic ids
  if (raw) return(responses)

  # parse the responses
  responses_dt <- parallel::mclapply(ids, function(name) {
    resp <- responses[[name]]
    response_dt <- switch(
      httr2::resp_content_type(resp),
      "text/tab-separated-values" = parse_cellosaurus_tsv(resp, name),
      "text/plain" = parse_cellosaurus_text(resp, name, parsed, keep_duplicates),
      .err("Response content type is not 'text/tab-separated-values' or 'text/plain'")
    )
    response_dt
    }) |> data.table::rbindlist(fill = TRUE)

  return(responses_dt)

}

#' parse responses
#' 
#' @noRd 
#' @keywords internal
parse_cellosaurus_tsv <- function(resp, name){
  .err("DEPRECATED: TSV parsing is not supported. Please use the txt parser.")
}

#' parse responses
#' 
#' @noRd 
#' @keywords internal
parse_cellosaurus_text <- function(resp, name, parsed, keep_duplicates = FALSE){
  lines <- httr2::resp_body_string(resp)  |>
            strsplit("\n") |> 
            unlist()
  
  x <- 
    Map(
      f = function(lines, i, j) {
          lines[i:(j - 1L)]
      },
      i = grep(pattern = "^ID\\s+", x = lines, value = FALSE),
      j = grep(pattern = "^//$", x = lines, value = FALSE),
      MoreArgs = list("lines" = lines),
      USE.NAMES = FALSE
    )
  
  if(length(x) == 0L){
    .warn(paste0("No results found for ", name))
    result <- data.table::data.table()
    result$query <- name
    return(result)
  }
  responses_dt <- parallel::mclapply(
      X = x,
      FUN = .processEntry
  ) |> data.table::rbindlist(fill = TRUE)

  responses_dt <- .formatSynonyms(responses_dt)
  if(parsed) return(responses_dt)
  query <- name
  name <- cleanCharacterStrings(name)
  # If theres an EXACT match 
  if(any(responses_dt$cellLineName == query)){
    data.table::setkeyv(responses_dt, "cellLineName")
    result <- responses_dt[query]
  } else{
    result <- responses_dt[matchNested(name, responses_dt, keep_duplicates = keep_duplicates)]
  }

  result$query <-query 
  result <- result[, c("cellLineName", "accession", "query")]

  if (nrow(result) == 0L) {
    .warn(paste0("No results found for ", query))
    result <- data.table::data.table()
    result$query <-query 
    return(result)
  }else{
    return(result)
  }

}


#' Format the `synonyms` column
#'
#' @note Updated 2023-01-24.
#' @noRd
.formatSynonyms <- function(responses_dt) {
  .splitCol(
      object = responses_dt,
      colName = "synonyms",
      split = "; "
  )
}

#' Split a column into a character list
#'
#' @note Updated 2023-09-22.
#' @noRd
.splitCol <- function(object, colName, split = "; ") {
  checkmate::assert_class(object, "data.table")
  object[[colName]] <- strsplit(object[, get(colName)], split = split, fixed = TRUE)
  object
}



## This function processes an entry in the cellosaurus database.
## It splits the input string, organizes the data into a nested list,
## handles optional keys, removes discontinued identifiers from the DR field,
## and converts the resulting list into a data table.
.processEntry <- function(
  x, 
  requiredKeys = c("AC", "CA", "DT", "ID"),
  nestedKeys = c("CC", "DI", "DR", "HI", "OI", "OX", "RX", "ST", "WW"),
  optionalKeys = c("AG", "SX", "SY")
) {
  x <- strSplit(x, split = "   ")

  x <- split(x[, 2L], f = x[, 1L])

  # create a single row dt from the list
  dt <- data.table::data.table(
    ID = x[["ID"]],
    AC = x[["AC"]]
  )

  for (name in setdiff(names(requiredKeys), c("ID", "AC"))) {
    dt[[name]] <- x[[name]]
  }

  for (key in optionalKeys) {
    dt[[key]] <- ifelse(is.null(x[[key]]), NA_character_, x[[key]])
  }
  for (key in nestedKeys) {
    dt[[key]] <- ifelse(is.null(x[[key]]), NA_character_, unique(x[[key]]))
  }

  ## Filter out discontinued identifiers from DR (e.g. "CVCL_0455").
  discontinued <- grep(
    pattern = "Discontinued:",
    x = x[["CC"]],
    fixed = TRUE,
    value = TRUE
  )
  if (isTRUE(length(discontinued) > 0L)) {
    discontinued <- sub(
      pattern = "^Discontinued: (.+);.+$",
      replacement = "\\1",
      x = discontinued
    )
    dt[["DR"]] <- setdiff(x = x[["DR"]], y = discontinued)
  }
  # create data.table of lists
  responses_dt <- dt

  old_names <- c("AC", "AG", "AS", "CA", "CC", "DI", "DR", "DT", "HI", "ID", 
            "OI", "OX", "RX", "ST", "SX", "SY", "WW")

  new_names <- c("accession", "ageAtSampling", "secondaryAccession", "category", 
    "comments", "diseases", "crossReferences", "date", "hierarchy", "cellLineName",
    "originateFromSameIndividual", "speciesOfOrigin", "referencesIdentifiers", 
    "strProfileData", "sexOfCell", "synonyms", "webPages")
      
  data.table::setnames(responses_dt, old = old_names, new = new_names, skip_absent = TRUE)
  responses_dt
}


