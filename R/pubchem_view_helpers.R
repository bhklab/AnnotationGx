#' Get all available annotation headings
#'
#' https://pubchem.ncbi.nlm.nih.gov/rest/pug/annotations/headings/JSON will return a list of all available headings
#'
#' @keywords internal
#' @noRd
.get_all_heading_types <- function() {
  url <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug/annotations/headings/JSON"
  req <- .build_pubchem_request(url)
  response <- httr2::req_perform(req) |> .parse_resp_json()
  .asDT(response[[1]][[1]])
}


#' Build a PubChem REST query URL
#'
#' pubchem<DOT>ncbi<DOT>nlm<DOT>nih<DOT>gov/rest/pug_view/<ANNOTATION>/<RECORD>/<ID>/<OUTPUT><?OPTIONS>
#'
#' Optional Parameters:
#'   - annotation: data, index, annotations, categories, neighbors, literature, structure, image, qr, linkout
#'  - record: compound, substance, assay, cell, gene, protein
#' - filter (Parameter, Value, Description):
#'      - Page, n <integer>, The page number to retrieve. Retrieves the nth page of the output data when the data is paginated.
#'          This option is useful when downloading the annotations under a specific heading for all compounds
#'          By default, retrieve all pages of the output data.
#'      - Version, n <int> for substance | n.m <int.int> for assay, data for a particular version of a substance or assay record
#'      - Heading, (Sub)heading name, The name of the annotation heading or subheading to retrieve
#'      - Source, source name, The name of the annotation source to retrieve from a specified source
#'
#' @param id The identifier for the query.
#' @param annotation The type of annotation to retrieve. Options include "data", "index", "annotations", "categories", "neighbors", "literature", "structure", "image", "qr", or "linkout".
#' @param record The type of record to retrieve. Options include "compound", "substance", "assay", "cell", "gene", or "protein".
#' @param page The page number to retrieve. Retrieves the nth page of the output data when the data is paginated. By default, retrieve all pages of the output data.
#' @param version The version of the record to retrieve. For substance, this is an integer. For assay, this is a string in the format "n.m".
#' @param heading The name of the annotation heading or subheading to retrieve.
#' @param source The name of the annotation source to retrieve from a specified source.
#' @param output The desired output format. Options are "JSON", "XML", "SDF", "TXT", "CSV".
#'
#' @return The query URL
#'
#' @keywords internal
#' @noRd
.build_pubchem_view_query <- function(
    id, annotation = "data", record = "compound", page = NULL, version = NULL, heading = NULL, source = NULL, output = "JSON", ...) {
  funContext <- .funContext(".build_pubchem_view_query")


  checkmate::assert_choice(
    annotation,
    c("data", "index", "annotations", "categories", "neighbors", "literature", "structure", "image", "qr", "linkout")
  )
  checkmate::assert_choice(record, c("compound", "substance", "assay", "cell", "gene", "protein"))

  opts_ <- list()
  if (!is.null(heading)) {
    if (record == "substance") {
      .debug(
        funContext,
        " fyi: https://pubchem.ncbi.nlm.nih.gov/rest/pug/annotations/headings/JSON
                 has no substance headings"
      )
    } else {
      check <- checkmate::check_character(
        unique(getPubchemAnnotationHeadings(record, heading)$Heading),
        min.chars = 1, min.len = 1
      )
      if (!isTRUE(check)) {
        .err(
          funContext, "Invalid heading: ", heading,
          ". Use getPubchemAnnotationHeadings() to get valid headings."
        )
      }
    }
    opts_ <- c(opts_, list(heading = heading))
  }
  if (!is.null(version)) {
    if (record == "substance") {
      checkmate::assert_string(version, min.chars = 1)
    } else {
      checkmate::assert_numeric(version, lower = 1)
    }
    opts_ <- c(opts_, list(version = version))
  }

  if (!is.null(source)) {
    checkmate::assert_string(source, min.chars = 1)
    opts_ <- c(opts_, list(source = source))
  }

  if (!is.null(page)) {
    checkmate::assert_numeric(page, lower = 1)
    opts_ <- c(opts_, list(page = page))
  }

  base_url <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug_view"
  url <- httr2::url_parse(base_url)
  url$path <- .buildURL(url$path, annotation, record, id, output)
  url$query <- opts_

  url |>
    httr2::url_build() |>
    .build_pubchem_request()
}

#' Generic function to parse one of the annotation helpers
#'
#' @noRd
#' @keywords internal
.clean_parsed_annotation <- function(result) {
  # If returned value is a list, concatenate the elements into a single string with ";"
  if (length(result) > 1) {
    return(paste(result, collapse = "; "))
  }
  return(result)
}

#' Parses the JSON response from an HTTP request.
#'
#' @noRd
#' @keywords internal
.parseCHEMBLresponse <- function(result) {
  gsub("Compound::", "", result[["Record"]][["Reference"]][["SourceID"]]) |>
    .clean_parsed_annotation()
}

#' Parses the JSON response from an HTTP request.
#'
#' @noRd
#' @keywords internal
.parseCASresponse <- function(result) {
  df <- result[["Record"]][["Reference"]]
  df[df$SourceName == "CAS Common Chemistry", "SourceID"] |>
    .clean_parsed_annotation()
}


#' Parses the JSON response from an HTTP request.
#'
#' @noRd
#' @keywords internal
.parseNSCresponse <- function(result) {
  df <- result[["Record"]][["Reference"]]
  df[df$SourceName == "DTP/NCI", "SourceID"] |>
    .clean_parsed_annotation()
}


#' Parses the JSON response from an HTTP request.
#'
#' @noRd
#' @keywords internal
.parseATCresponse <- function(result) {
  df <- result[["Record"]][["Reference"]]
  df[df$SourceName == "WHO Anatomical Therapeutic Chemical (ATC) Classification", "SourceID"] |>
    .clean_parsed_annotation()
}


#' Parses the JSON response from an HTTP request.
#'
#' @noRd
#' @keywords internal
.parseDILIresponse <- function(result) {
  df <- result[["Record"]][["Reference"]]
  df[df$SourceName == "Drug Induced Liver Injury Rank (DILIrank) Dataset", "SourceID"] |>
    .clean_parsed_annotation()
}
