#' Get the list of fields in the Cellosaurus schema
#'
#' This function retrieves the list of fields available in the Cellosaurus schema.
#' It internally calls the `.cellosaurus_schema()` function to fetch the schema
#' and extracts the list of fields from it.
#'
#' @param common Logical indicating whether to return only the common fields. Default is FALSE.
#' @param upper Logical indicating whether to return the fields in uppercase. Default is FALSE.
#'
#' @return A character vector containing the list of fields in the Cellosaurus schema.
#'
#' @examples
#' cellosaurus_fields()
#' cellosaurus_fields(common = TRUE)
#' cellosaurus_fields(upper = TRUE)
#'
#' @export
cellosaurus_fields <- function(common = FALSE, upper = FALSE) {
  if(common == TRUE) {
    fields <- c("id", "ac", "acas", "sy", "dr", "di", "din", "dio", "ox", "cc",
    "sx", "ag", "oi", "hi", "ch", "ca",  "dt", "dtc", "dtu", "dtv", "from", "group")
  } else{
    schema <- .cellosaurus_schema()
    fields <- schema$components$schemas$Fields$enum
  }

  if(upper == TRUE) {
    fields <- toupper(fields)
  }else{
    fields <- tolower(fields)
  }

  return(fields)
}

#' Get the Cellosaurus API version
#'
#' This function retrieves the version of the Cellosaurus API.
#'
#' @return The version of the Cellosaurus API.
#'
#' @examples
#' cellosaurusAPIVersion()
#'
#' @export
cellosaurusAPIVersion <- function() {
  .cellosaurus_schema()$info$version
}

# fields <- c("AC", "CA", "DT", "ID", "DI", "DR", "HI", "OI", "OX", "AG", "SX", "SY") |> tolower()

#' Map Cell Line IDs to Accession Numbers
#'
#' This function maps cell line IDs to accession numbers using the Cellosaurus database.
#'
#' @param ids A character vector of cell line IDs.
#' @param numResults The maximum number of results to return for each query. Default is 1000.
#' @param from The type of input IDs. Possible values are "idsy" (default), "ac", "id", "sy", and "misspelling".
#' @param sort The sorting order of the results. Possible values are "ac" (default), "id", "sy", and "misspelling".
#' @param keep_duplicates Logical indicating whether to keep duplicate results. Default is FALSE.
#' @param fuzzy Logical indicating whether to perform a fuzzy search. Default is FALSE.
#' @param query_only Logical indicating whether to return only the query URLs. Default is FALSE.
#' @param raw Logical indicating whether to return the raw HTTP responses. Default is FALSE.
#' @param parsed Logical indicating whether to parse the response text. Default is TRUE.
#' @param ... Additional arguments to be passed to the underlying functions.
#'
#' @return A data.table containing the mapped cell line IDs and accession numbers.
#'
#' @examples
#' mapCell2Accession(ids = c("A549", "MCF7"))
#'
#' @export
mapCell2Accession <- function(
    ids, numResults = 10000, from = "idsy", sort = "ac", keep_duplicates = FALSE, 
    fuzzy = FALSE, query_only = FALSE, raw = FALSE, parsed = TRUE, ...
) {

  funContext <- .funContext("mapCell2Accession")

  # Input validation and coercion
  if (!is.character(ids)) {
    .warn("Input names are not character, coercing to character")
    ids <- as.character(ids)
  }

  to = c("ac", "id", "sy", "misspelling", "dr", "cc", "ca", "di", "ag", "sx", "hi")

  # create query list
  .info(funContext, "Creating Cellosaurus queries")

  queries <- .create_cellosaurus_queries(ids, from, fuzzy)
  names(queries) <- ids

  .info(funContext, "Building Cellosaurus requests")
  # build the list of requests
  requests <- parallel::mclapply(queries, function(query) {
    .build_cellosaurus_request(
      query = query,
      to = to,
      numResults = numResults,
      sort = sort,
      output = "TXT",
      fuzzy = fuzzy,
      ...
    )
  })

  if (query_only) return(lapply(requests, function(req) req$url))
  
  # perform the requests
  .info(funContext, "Performing Cellosaurus queries")
  responses <- .perform_request_parallel(requests)
  names(responses) <- as.character(ids) # in case its an numeric ID  like cosmic ids
  if (raw) return(responses)

  # parse the responses
  .info(funContext, "Parsing Cellosaurus responses")
  responses_dt <- parallel::mclapply(ids, function(name) {
    resp <- responses[[name]]

    resp <- .parse_cellosaurus_lines(resp)
    if(length(resp) == 0L){
      .warn(paste0("No results found for ", name))
      result <- data.table::data.table()
      result$query <- name
      return(result)
    }
    response_dt <- .parse_cellosaurus_text(resp, name, parsed, keep_duplicates)
    response_dt
  }) 
  

  responses_dt <- data.table::rbindlist(responses_dt, fill = TRUE)

  return(responses_dt)

}


#' Parses the lines of a cellosaurus response
#'
#' This function takes a response object and parses the lines of the response
#' to extract specific sections of the cellosaurus data.
#'
#' @param resp The response object containing the cellosaurus data
#' @return A list of parsed lines from the cellosaurus data
#' 
#' @keywords internal
#' @noRd
.parse_cellosaurus_lines <- function(resp){
  lines <- httr2::resp_body_string(resp)  |>
            strsplit("\n") |> 
            unlist()
  
  Map(
    f = function(lines, i, j) {
        lines[i:(j - 1L)]
    },
    i = grep(pattern = "^ID\\s+", x = lines, value = FALSE),
    j = grep(pattern = "^//$", x = lines, value = FALSE),
    MoreArgs = list("lines" = lines),
    USE.NAMES = FALSE
  )
  
}

#' parse responses
#' 
#' @noRd 
#' @keywords internal
.parse_cellosaurus_text <- function(resp, name, parsed = FALSE, keep_duplicates = FALSE){

  responses_dt <- lapply(
      X = resp,
      FUN = .processEntry
  ) 
  tryCatch({
    responses_dt <- data.table::rbindlist(responses_dt, fill = TRUE)
  }, error = function(e) {
    .err(paste0("Error parsing response for ", name, ": ", e$message))
  }) 

  responses_dt <- .formatSynonyms(responses_dt)

  if(!parsed) {
    responses_dt$query <- name
    return(responses_dt[, c("cellLineName", "accession", "query")])
  }


  result <- .find_cellosaurus_matches(responses_dt, name, keep_duplicates)
  result$query <- name 
  result <- result[, c("cellLineName", "accession", "query")]

  return(result)

}

#' Splits cellosaurus lines into a named list
#'
#' This function takes a vector of cellosaurus lines and splits them into a named list.
#' The lines are split based on the delimiter "   " (three spaces).
#'
#' @param lines A vector of cellosaurus lines
#' @return A named list where each element corresponds to a unique identifier and contains the associated lines
#' @examples
#' lines <- c("ID1   Line 1", "ID1   Line 2", "ID2   Line 1", "ID2   Line 2")
#' AnnotationGx:::.split_cellosaurus_lines(lines)
#' # Output:
#' # $ID1
#' # [1] "Line 1" "Line 2"
#' #
#' # $ID2
#' # [1] "Line 1" "Line 2"
#'
#' @noRd
.split_cellosaurus_lines <- function(lines){
  x <- strSplit(lines, split = "   ")
  x <- split(x[, 2L], f = x[, 1L])
  x
}


## This function processes an entry in the cellosaurus database.
## It splits the input string, organizes the data into a nested list,
## handles optional keys, removes discontinued identifiers from the DR field,
## and converts the resulting list into a data table.
.processEntry <- function(x){
  requiredKeys = c("AC", "CA", "DT", "ID")
  nestedKeys = c("DI", "DR", "HI")
  optionalKeys = c("AG", "SX", "SY")
  specialKeys = c("CC")

  x <- .split_cellosaurus_lines(x)
  
  if("CC" %in% names(x)){
    x <- .formatComments(x)
  }

  # create a single row dt from the list
  dt <- data.table::data.table(
    ID = x[["ID"]],
    AC = x[["AC"]]
  )

  for (name in setdiff(requiredKeys, c("ID", "AC"))) {
    dt[[name]] <- x[[name]]
  }
  for (key in optionalKeys) {
    dt[[key]] <- ifelse(
      is.null(x[[key]]), 
      NA_character_, 
      x[[key]]
    )
  }
  for (key in nestedKeys) {
    dt[[key]]  <- ifelse(
      is.null(x[[key]]),
      NA_character_,
      list(.splitNestedCol(x, key, "; ")[[key]])
    )
  }
  for (key in specialKeys) {
    dt[[key]] <- ifelse(
      is.null(x[[key]]),
      NA_character_,
      x[key]
    )
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
    dt[["DR"]] <- list(setdiff(x = x[["DR"]], y = discontinued))
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



#' Find Cellosaurus Matches
#'
#' This function searches for matches in a data table based on a given name.
#' It first tries to find an exact match as the cellLineName to avoid cases where
#' the first row is the wrong cell line but the query is in a synonym, and the second row
#' is the correct cell line. If an exact match is not found, it searches for matches
#' in the data table using the query and cleaned name. If no matches are found, it
#' creates an empty data table with the columns "cellLineName", "accession", and "query".
#'
#' @param responses_dt A data table containing the responses.
#' @param name The name to search for.
#' @param keep_duplicates A logical value indicating whether to keep duplicate matches.
#'
#' @return A data table with the matched results, or an empty data table if no matches are found.
#'
#' @examples
#' responses_dt <- data.table::data.table(
#'   cellLineName = c("Cell Line 1", "Cell Line 2", "Cell Line 3"),
#'   accession = c("Accession 1", "Accession 2", "Accession 3"),
#'   synonyms = list(c("Synonym 1", "Synonym 2"), c("Synonym 3"), c("Synonym 4"))
#' )
#' 
#' .find_cellosaurus_matches(responses_dt, "Cell Line 2")
#'
#' @noRd
#' @keywords internal
.find_cellosaurus_matches <- function(
  responses_dt, 
  name, 
  keep_duplicates = FALSE
){
  # save original name
  query <- name
  name <- cleanCharacterStrings(name)

  # first try for exact match as cellLineName to avoid the case where
  # the first row is the wrong cellline but the query is in a synonym
  # but the second row is the correct cellline
  # TODO:: REFACTOR THIS TO NOT REPEAT THE CONDITIONAL 
  if(any(responses_dt$cellLineName == query)){
    data.table::setkeyv(responses_dt, "cellLineName")
    result <- responses_dt[query]
  } else if(length(matchNested(query, responses_dt, keep_duplicates = keep_duplicates)) > 0){
    matches <- matchNested(query, responses_dt, keep_duplicates = keep_duplicates)
    result <- responses_dt[matches]
  } else if(length(matchNested(name, responses_dt, keep_duplicates = keep_duplicates)) > 0){
    matches <- matchNested(name, responses_dt, keep_duplicates = keep_duplicates)
    result <- responses_dt[matches]
  } else if(any(cleanCharacterStrings(responses_dt$cellLineName) == name)){
    matches <- cleanCharacterStrings(responses_dt$cellLineName) == name
    result <- responses_dt[matches][1]
  } else if(length(matchNested(name, lapply(responses_dt$synonyms, cleanCharacterStrings)))> 0 ){
    matches <- matchNested(name, lapply(responses_dt$synonyms, cleanCharacterStrings))
    result <- responses_dt[matches]
  } else{
    .warn(paste0("No results found for ", query))
    # create an empty data.table with the following columns:
    # c("cellLineName", "accession", "query")
    result <- data.table::data.table(
      cellLineName = NA_character_,
      accession = NA_character_,
      query = query
    )
  }
  return(result)
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


#' Format the `comments` column
#'
#' @note Updated 2023-09-22.
#' @noRd
.formatComments <- function(object) {
    test_ <- strSplit(object[["CC"]], ": ", n = 2)
    test_ <- split(test_[, 2L], f = test_[, 1L])

    test_ <- sapply(test_, strsplit, split = "; ")

    object[["CC"]] <- test_
    object
}