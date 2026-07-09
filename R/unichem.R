# Unichem API documentation: https://www.ebi.ac.uk/unichem/info/webservices

#' Get the list of sources in UniChem.
#'
#' @param all_columns `boolean` Whether to return all columns. Defaults to FALSE.
#'

#'
#' Returns a `data.table` with the following columns:
#' - `CompoundCount` (integer): Total of compounds provided by that source
#' - `BaseURL` (string): Source Base URL for compounds
#' - `Description` (string): Source database description
#' - `LastUpdated` (string): Date in which the source database was last updated
#' - `Name` (string): Short name of the source database
#' - `NameLabel` (string): Machine readable label name of the source database
#' - `NameLong` (string): Full name of the source database
#' - `SourceID` (integer): Unique ID for the source database
#' - `Details` (string): Notes about the source
#' - `ReleaseDate` (string): Date in which the source database was released
#' - `ReleaseNumber` (integer): Release number of the source database data stored in UniChEM
#' - `URL` (string): Main URL for the source
#' - `UpdateComments` (string): Notes about the update process of that source to UniChEM
#'
#'
#' @return A data.table with the list of sources in UniChem.
#' @examples
#' # Requires internet connection to UniChem
#' if (interactive()) {
#'   getUnichemSources()
#' }
#'
#' @export
getUnichemSources <- function(all_columns = FALSE) {
  funContext <- .funContext("AnnotationGx::getUnichemSources")
  sources_dt <- .cache_fetch(
    namespace = "unichem/sources",
    params = list(all_columns = TRUE),
    FUN = function() {
      request <- .build_unichem_query("sources") |>
        .build_request()
      response <- request |>
        .perform_request() |>
        .parse_unichem_response(
          request_label = "UniChem sources",
          request = request
        )

      if (response$response != "Success") {
        .err(funContext, "Unichem API request failed.")
      }

      .debug(
        funContext,
        sprintf("Unichem sourceCount: %s", response$totalSources)
      )

      sources_dt <- .asDT(response$sources)

      old_names <- c(
        "UCICount",
        "baseIdUrl",
        "description",
        "lastUpdated",
        "name",
        "nameLabel",
        "nameLong",
        "sourceID",
        "srcDetails",
        "srcReleaseDate",
        "srcReleaseNumber",
        "srcUrl",
        "updateComments"
      )

      new_names <- c(
        "CompoundCount",
        "BaseURL",
        "Description",
        "LastUpdated",
        "Name",
        "NameLabel",
        "NameLong",
        "SourceID",
        "Details",
        "ReleaseDate",
        "ReleaseNumber",
        "URL",
        "UpdateComments"
      )

      data.table::setnames(sources_dt, old_names, new_names)

      new_order <- c(
        "Name",
        "NameLabel",
        "NameLong",
        "SourceID",
        "CompoundCount",
        "BaseURL",
        "URL",
        "Details",
        "Description",
        "ReleaseNumber",
        "ReleaseDate",
        "LastUpdated",
        "UpdateComments"
      )

      sources_dt[, new_order, with = FALSE]
    }
  )

  if (all_columns) {
    return(sources_dt)
  }

  sources_dt[, c("Name", "SourceID")]
}

.parse_unichem_response <- function(
  response,
  request_label,
  request = NULL,
  max_attempts = 3L,
  retry_delay = 1
) {
  attempt <- 1L
  current_response <- response

  while (attempt <= max_attempts) {
    parsed <- tryCatch(
      .parse_resp_json(current_response),
      error = identity
    )

    if (!inherits(parsed, "error")) {
      return(parsed)
    }

    content_type <- tryCatch(
      httr2::resp_content_type(current_response),
      error = function(...) "unknown"
    )

    if (
      !is.null(request) &&
        content_type != "application/json" &&
        attempt < max_attempts
    ) {
      Sys.sleep(retry_delay)
      current_response <- request |>
        .perform_request()
      attempt <- attempt + 1L
      next
    }

    status <- tryCatch(
      httr2::resp_status(current_response),
      error = function(...) {
        NA_integer_
      }
    )
    body_preview <- tryCatch(
      substr(httr2::resp_body_string(current_response), 1, 200),
      error = function(...) "<response body unavailable>"
    )

    .err(
      .funContext("AnnotationGx::queryUnichemCompound"),
      "UniChem returned a non-JSON response for ",
      request_label,
      " (status: ",
      status,
      ", content type: ",
      content_type,
      "). Body preview: ",
      body_preview
    )
  }
}

#' Query UniChem for a compound.
#'
#' This function queries the UniChem API for a compound based on the provided parameters.
#'
#' @param compound `character`, `integer`, or a list of such values. When a vector
#'   or list is supplied, each element is queried and the results are returned as
#'   a named list.
#' @param type `character` The type of compound identifier to search for. Valid types are "uci", "inchi", "inchikey", and "sourceID".
#' @param sourceID `integer` The source ID to search for if the type is "sourceID".
#'   When querying multiple compounds, this can be a vector the same length as
#'   `compound` or a single value recycled to all queries. Defaults to `NA`.
#' @param request_only `boolean` Whether to return the request only. Defaults to FALSE.
#' @param raw `boolean` Whether to return the raw response. Defaults to FALSE.
#' @param progress `logical` or `character`. Passed through to
#'   `.perform_request_parallel()` when multiple compounds are supplied. Use a
#'   character string to customise the progress label. Defaults to
#'   `"Querying UniChem..."`.
#' @param ... Additional arguments.
#'
#' @return For a single query, a list with the external mappings and the UniChem
#'   mappings. For multiple queries, a named list of such results (one per
#'   compound). If `raw = TRUE`, raw responses are returned instead.
#'
#' @examples
#' if (interactive()) {
#'   queryUnichemCompound(
#'     type = "sourceID",
#'     compound = "444795",
#'     sourceID = 22
#'   )
#' }
#'
#' @export
queryUnichemCompound <- function(
  compound,
  type,
  sourceID = NA_integer_,
  request_only = FALSE,
  raw = FALSE,
  progress = "Querying UniChem...",
  ...
) {
  checkmate::assert_string(type)
  checkmate::assert_flag(request_only)
  checkmate::assert_flag(raw)
  checkmate::assert(
    checkmate::check_flag(progress),
    checkmate::check_string(progress, min.chars = 1)
  )

  compounds <- if (is.list(compound)) {
    unlist(compound, recursive = TRUE, use.names = TRUE)
  } else {
    compound
  }
  checkmate::assert_atomic_vector(compounds, min.len = 1)

  many_queries <- length(compounds) > 1

  validate_source_ids <- function(src_ids) {
    if (type != "sourceID") {
      return(rep(NA_integer_, length(compounds)))
    }

    checkmate::assert_integerish(
      src_ids,
      lower = 1,
      any.missing = FALSE
    )

    if (length(src_ids) == 1L) {
      src_ids <- rep(src_ids, length(compounds))
    } else if (length(src_ids) != length(compounds)) {
      stop(
        "`sourceID` must be length 1 or match the number of compounds ",
        "when type = 'sourceID'"
      )
    }
    checkmate::assert_integerish(
      src_ids,
      lower = 1,
      any.missing = FALSE
    )
    src_ids
  }

  build_request <- function(cmp, src) {
    .build_unichem_compound_req(
      type = type,
      compound = cmp,
      sourceID = if (is.na(src)) NULL else src,
      ...
    )
  }

  parse_response <- function(parsed, cmp_label) {
    if (parsed$response != "Success") {
      msg <- paste(
        "Unichem API request failed for compound",
        cmp_label,
        "with type",
        type,
        ". Error:",
        parsed$error
      )
      .err(.funContext("AnnotationGx::queryUnichemCompound"), msg)
    }

    mapped_sources_dt <- .asDT(parsed$compounds$sources)
    old_names <- c("compoundId", "shortName", "longName", "id", "url")
    new_names <- c(
      "compoundID",
      "Name",
      "NameLong",
      "sourceID",
      "sourceURL"
    )
    data.table::setnames(
      mapped_sources_dt,
      old = old_names,
      new = new_names
    )

    External_Mappings <- mapped_sources_dt[, new_names, with = FALSE]

    UniChem_Mappings <- list(
      UniChem.UCI = parsed$compounds$uci,
      UniChem.InchiKey = parsed$compounds$standardInchiKey,
      UniChem.Inchi = parsed$compounds$inchi$inchi,
      UniChem.formula = parsed$compounds$inchi$formula,
      UniChem.connections = parsed$compounds$inchi$connections,
      UniChem.hAtoms = parsed$compounds$inchi$hAtoms
    )

    list(
      External_Mappings = External_Mappings,
      UniChem_Mappings = UniChem_Mappings
    )
  }

  query_impl <- function() {
    source_ids <- validate_source_ids(sourceID)

    if (many_queries) {
      requests <- Map(build_request, compounds, source_ids)
      name_candidates <- names(compounds)
      if (
        !is.null(name_candidates) &&
          length(name_candidates) == length(compounds)
      ) {
        names(requests) <- name_candidates
      } else {
        names(requests) <- as.character(compounds)
      }

      if (request_only) {
        return(requests)
      }

      responses <- .perform_request_parallel(requests, progress = progress)
      names(responses) <- names(requests)

      parsed_responses <- Map(
        function(response, request, cmp_label) {
          .parse_unichem_response(
            response,
            paste0("compound `", cmp_label, "` with type `", type, "`"),
            request = request
          )
        },
        responses,
        requests,
        names(responses)
      )

      if (raw) {
        names(parsed_responses) <- names(responses)
        return(parsed_responses)
      }

      results <- Map(
        function(parsed, cmp_label) {
          tryCatch(
            parse_response(parsed, cmp_label),
            error = function(e) {
              structure(
                list(error = conditionMessage(e)),
                class = c("unichem_error", "list")
              )
            }
          )
        },
        parsed_responses,
        names(responses)
      )

      names(results) <- names(responses)
      return(results)
    }

    request <- build_request(compounds[[1L]], source_ids[[1L]])
    if (request_only) {
      return(request)
    }

    response <- request |>
      .perform_request()
    parsed <- .parse_unichem_response(
      response,
      paste0("compound `", compounds[[1L]], "` with type `", type, "`"),
      request = request
    )

    if (raw) {
      return(parsed)
    }

    parse_response(parsed, compounds[[1L]])
  }

  if (request_only || raw) {
    return(query_impl())
  }

  .cache_fetch(
    namespace = "unichem/query",
    params = list(
      compound = compounds,
      type = type,
      sourceID = sourceID,
      extra = list(...)
    ),
    FUN = query_impl
  )
}
