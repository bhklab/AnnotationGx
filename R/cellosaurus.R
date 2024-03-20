#' Maps cell line names to accession numbers
#'
#' This function takes a vector of cell line names and maps them to accession numbers
#' using the Cellosaurus database. It performs a parallel request to retrieve the
#' mapping information and returns a data table with the results.
#'
#' @param ids A character vector of cell line names.
#' @param numResults The number of results to return for each query. Default is 1.
#' @param from The field to query from. Default is "idsy".
#' @param to The field to query to. Default is both "id" and "ac".
#' @param sort The order in which to return the results. Default is NULL.
#' @param query_only If TRUE, returns the query URL instead of the results. Default is FALSE.
#' @param raw If TRUE, returns the raw response instead of a data table. Default is FALSE.
#' @param BPPARAM A BiocParallel parameter object controlling the parallelization.
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
      "text/tab-separated-values" = parse_cellosaurus_tsv(resp, queries, name),
      "text/plain" = parse_cellosaurus_text(resp, name, parsed, keep_duplicates),
      .err("Response content type is not 'text/tab-separated-values' or 'text/plain'")
    )
    response_dt
    }) |> data.table::rbindlist(fill = TRUE)

  return(responses_dt)

}

fields <- fields <- c("AC", "CA", "DT", "ID", "DI", "DR", "HI", "OI", "OX", "AG", "SX", "SY") |> tolower()

parse_cellosaurus_tsv <- function(resp, name){
  .err("DEPRECATED: TSV parsing is not supported. Please use the txt parser.")
}

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
  
  requiredKeys <- c("AC", "CA", "DT", "ID")
  nestedKeys <- c("CC", "DI", "DR", "HI", "OI", "OX", "RX", "ST", "WW")
  optionalKeys <- c("AG", "SX", "SY") #"AS",
  responses_dt <- parallel::mclapply(
      X = x,
      FUN = .processEntry,
      requiredKeys = requiredKeys,
      nestedKeys = nestedKeys,
      optionalKeys = optionalKeys
  ) |> data.table::rbindlist(fill = TRUE)

  responses_dt <- .formatSynonyms(responses_dt)
  # if(parsed) return(responses_dt)
  query <- name
  name <- cleanCharacterStrings(name)
  # If theres an EXACT match 
  if(any(responses_dt$cellLineName == query)){
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
.processEntry <- function(x, requiredKeys, nestedKeys, optionalKeys) {

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



strSplit <- function(x, split, fixed = TRUE, n = Inf) {

    if (is.finite(n)) {
        x <- .strSplitFinite(x = x, split = split, n = n, fixed = fixed)
    } else {
        x <- .strSplitInfinite(x = x, split = split, fixed = fixed)
    }
    n2 <- lengths(x)
    assert(
        length(unique(n2)) == 1L,
        msg = sprintf(
            "Split mismatch detected: %s.",
            toString(which(n2 != n2[[1L]]))
        )
    )
    n2 <- n2[[1L]]
    x <- unlist(x = x, recursive = FALSE, use.names = FALSE)
    x <- matrix(data = x, ncol = n2, byrow = TRUE)
    x
}




#' Split a string into a finite number of capture groups
#'
.strSplitFinite <- function(x, split, n, fixed) {

    checkmate::assertString(split)
    checkmate::assertFlag(fixed)
    checkmate::assert_integerish(n, lower = 2L, upper = Inf)
    checkmate::assert_character(x)

    m <- gregexpr(pattern = split, text = x, fixed = fixed)
    ln <- lengths(m)
    assert(
        all((ln + 1L) >= n),
        msg = sprintf(
            "Not enough to split: %s.",
            toString(which((ln + 1L) < n))
        )
    )
    Map(
        x = x,
        m = m,
        n = n,
        f = function(x, m, n) {
            ml <- attr(m, "match.length")
            nl <- seq_len(n)
            m <- m[nl]
            ml <- ml[nl]
            out <- substr(x = x, start = 1L, stop = m[[1L]] - 1L)
            i <- 1L
            while (i < (length(m) - 1L)) {
                out <- append(
                    x = out,
                    values = substr(
                        x = x,
                        start = m[[i]] + ml[[i]],
                        stop = m[[i + 1L]] - 1L
                    )
                )
                i <- i + 1L
            }
            out <- append(
                x = out,
                values = substr(
                    x = x,
                    start = m[[n - 1L]] + ml[[n - 1L]],
                    stop = nchar(x)
                )
            )
            out
        },
        USE.NAMES = FALSE
    )
}




#' Split a string into an finite number of capture groups
.strSplitInfinite <- function(x, split, fixed) {
    checkmate::assertCharacter(x)
    checkmate::assertString(split)
    checkmate::assertFlag(fixed)
    strsplit(x = x, split = split, fixed = fixed)
}