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
#'
#' @export
getUnichemSources <- function(all_columns = FALSE) {
  funContext <- .funContext("AnnotationGx::getUnichemSources")

  response <- .build_unichem_query("sources") |>
    .build_request() |>
    .perform_request() |>
    .parse_resp_json()

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

  sources_dt <- sources_dt[, new_order, with = FALSE]

  if (all_columns) {
    return(sources_dt)
  }

  sources_dt[, c("Name", "SourceID")]
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
#' queryUnichemCompound(type = "sourceID", compound = "444795", sourceID = 22)
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

    checkmate::assert_integerish(src_ids, any.missing = FALSE)

    if (length(src_ids) == 1L) {
      src_ids <- rep(src_ids, length(compounds))
    } else if (length(src_ids) != length(compounds)) {
      stop(
        "`sourceID` must be length 1 or match the number of compounds ",
        "when type = 'sourceID'"
      )
    }
    checkmate::assert_integerish(src_ids, any.missing = FALSE)

    valid_ids <- getUnichemSources()$SourceID
    invalid_ids <- setdiff(unique(src_ids), valid_ids)
    if (length(invalid_ids) > 0L) {
      stop(
        "`sourceID` contains value(s) not available in UniChem: ",
        paste(invalid_ids, collapse = ", "),
        "\nValid source IDs: ",
        paste(valid_ids, collapse = ", ")
      )
    }
    src_ids
  }

  source_ids <- validate_source_ids(sourceID)

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

  if (many_queries) {
    requests <- Map(build_request, compounds, source_ids)
    name_candidates <- names(compounds)
    if (
      !is.null(name_candidates) && length(name_candidates) == length(compounds)
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

    parsed_responses <- Map(.parse_resp_json, responses)

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

  response <- request |> .perform_request()
  parsed <- .parse_resp_json(response)

  if (raw) {
    return(parsed)
  }

  parse_response(parsed, compounds[[1L]])
}
