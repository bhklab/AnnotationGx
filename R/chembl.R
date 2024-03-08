#' A general function for creating Queries to the ChEMBL API
#'
#' @description A general function for creating Queries to the ChEMBL API
#' www DOT ebi DOT ac DOT uk/chembl/api/data/ <resource>?<field>__<filter_type>=<value>
#' |       Resource Name       |                                                                                                   Description                                                                                                   |
#' |:-------------------------:|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|
#' | activity                  | Activity values recorded in an Assay                                                                                                                                                                            |
#' | assay                     | Assay details as reported in source Document/Dataset
#' | atc_class                 | WHO ATC Classification for drugs                                                                                                                                                                                |                                                                                                                                                           |
#' | binding_site              | WHO ATC Classification for drugs                                                                                                                                                                                |
#' | biotherapeutic            | Biotherapeutic molecules, which includes HELM notation and sequence data                                                                                                                                        |
#' | cell_line                 | Cell line information                                                                                                                                                                                           |
#' | chembl_id_lookup          | Look up ChEMBL Id entity type                                                                                                                                                                                   |
#' | compound_record           | Occurence of a given compound in a spcecific document                                                                                                                                                           |
#' | compound_structural_alert | Indicates certain anomaly in compound structure                                                                                                                                                                 |
#' | document                  | Document/Dataset from which Assays have been extracted                                                                                                                                                          |
#' | document_similarity       | Provides documents similar to a given one                                                                                                                                                                       |
#' | document_term             | Provides keywords extracted from a document using the TextRank algorithm                                                                                                                                        |
#' | drug                      | Approved drugs information, including (but not limited to) applicants, patent numbers and research codes. This endpoint aggregates data on the parent, please use the parent chembl id found in other endpoints |
#' | drug_indication           | Joins drugs with diseases providing references to relevant sources                                                                                                                                              |
#' | drug_warning              | Safety information for drugs withdrawn from one or more regions of the world and drugs that carry a warning for severe or life threatening adverse effects                                                      |
#' | go_slim                   | GO slim ontology                                                                                                                                                                                                |
#' | image                     | Graphical (svg) representation of Molecule                                                                                                                                                                      |
#' | mechanism                 | Mechanism of action information for approved drugs                                                                                                                                                              |
#' | metabolism                | Metabolic pathways with references                                                                                                                                                                              |
#' | molecule                  | Molecule information, including properties, structural representations and synonyms                                                                                                                             |
#' | molecule_form             | Relationships between molecule parents and salts                                                                                                                                                                |
#' | organism                  | Simple organism classification                                                                                                                                                                                  |
#' | protein_classification    | Protein family classification of TargetComponents                                                                                                                                                               |
#' | similarity                | Molecule similarity search                                                                                                                                                                                      |
#' | source                    | Document/Dataset source                                                                                                                                                                                         |
#' | status                    | API status with ChEMBL DB version number and API software version number                                                                                                                                        |
#' | substructure              | Molecule substructure search                                                                                                                                                                                    |
#' | target                    | Targets (protein and non-protein) defined in Assay                                                                                                                                                              |
#' | target_component          | Target sequence information (A Target may have 1 or more sequences)                                                                                                                                             |
#' | target_relation           | Describes relations between targets                                                                                                                                                                             |
#' | tissue                    | Tissue classification                                                                                                                                                                                           |
#' | xref_source               | Cross references to other resources for compounds                                                                                                                                                               |
#'
#'
#'
#' |        Filter Type       |                                                  Description                                                 |
#' |:------------------------:|:------------------------------------------------------------------------------------------------------------:|
#' | exact (iexact)           | Exact match with query (case insensitive equivalent)                                                         |
#' | contains (icontains)     | Wild card search with query (case insensitive equivalent)                                                    |
#' | startswith (istartswith) | Starts with query (case insensitive equivalent)                                                              |
#' | endswith (iendswith)     | Ends with query (case insensitive equivalent)                                                                |
#' | regex (iregex)           | Regular expression query (case insensitive equivalent)                                                       |
#' | gt (gte)                 | Greater than (or equal)                                                                                      |
#' | lt (lte)                 | Less than (or equal)                                                                                         |
#' | range                    | Within a range of values                                                                                     |
#' | in                       | Appears within list of query values                                                                          |
#' | isnull                   | Field is null                                                                                                |
#' | search                   | Special type of filter allowing a full text search based on elastic search queries                           |
#' | only                     | Select specific properties from the original endpoint and returns only the desired properties on each record |                                                                                                         |
#'
#' @param resource `character(1)` Resource to query
#' @param field `character(1)` Field to query
#' @param filter_type `character(1)` Filter type
#' @param value `character(1)` Value to query
#' @param format `character(1)` Format of the response
#'
#' @noRd
#' @keywords internal
.build_chembl_request <- function(
    resource,
    field = NULL, filter_type = NULL, value = NULL, format = "json") {
  # possible formats for now are XML, JSON and YAML
  checkmate::assert_choice(resource, c(.chembl_resources(), paste0(.chembl_resources(), "/schema")))
  checkmate::assert_choice(field, getChemblResourceFields(resource), null.ok = TRUE)
  checkmate::assert_choice(filter_type, .chembl_filter_types(), null.ok = TRUE)
  checkmate::assert_character(value, null.ok = TRUE)
  checkmate::assert_choice(format, c("json", "xml", "yaml"))

  # Construct the URL
  base_url <- .buildURL("https://www.ebi.ac.uk/chembl/api/data", resource)
  url <- httr2::url_parse(base_url)

  # Add the query parameters
  query <- list()
  query[["format"]] <- format
  fld <- paste0(field, "__", filter_type)
  query[[fld]] <- value
  url$query <- query

  final_url <- httr2::url_build(url)
  final_url |> .build_request()
}


#' Get ChEMBL Mechanism
#'
#' This function retrieves information about the mechanism of action for a given ChEMBL ID.
#'
#' @param chembl.ID The ChEMBL ID of the molecule.
#' @param resources The ChEMBL resource to query (default: "mechanism").
#' @param field The field to filter on (default: "molecule_chembl_id").
#' @param filter_type The filter type to use (default: "in").
#' @param returnURL Logical indicating whether to return the constructed URL (default: FALSE).
#' @param raw Logical indicating whether to return the raw response JSON (default: FALSE).
#'
#' @return A data.table containing the retrieved mechanism information.
#'
#' @examples
#' getChemblMechanism("CHEMBL1413")
#' getChemblMechanism("CHEMBL1413",
#'   resources = "mechanism", field = "molecule_chembl_id",
#'   filter_type = "in", returnURL = FALSE, raw = FALSE
#' )
#'
#' @export
getChemblMechanism <- function(
    chembl.ID, resources = "mechanism", field = "molecule_chembl_id", filter_type = "in",
    returnURL = FALSE, raw = FALSE) {
  # constructChemblQuery(resource = "mechanism", field = "molecule_chembl_id", filter_type = "in", value = "CHEMBL1413")
  # urls <- constructChemblQuery(resource = resources, field = field, filter_type = filter_type, value = chembl.ID)
  # urls <- URLencode(urls)
  response_dts <- lapply(chembl.ID, function(chembl.ID) {
    request <- .build_chembl_request(resource = resources, field = field, filter_type = filter_type, value = chembl.ID)

    if (returnURL) {
      return(request$url)
    }
    response <- .perform_request(request)

    response_json <- .parse_resp_json(response)
    if (raw) {
      return(response_json)
    }
    .asDT(response_json[["mechanisms"]])
  })

  if (returnURL || raw) {
    return(response_dts)
  }
  all_cols <- .chembl_mechanism_cols()
  # If any cols are missing, fill with NA
  response_dts <- lapply(response_dts, function(x) {
    missing_cols <- setdiff(all_cols, names(x))
    if (length(missing_cols) > 0) {
      x[, (missing_cols) := NA]
    }
    x
  })

  data.table::rbindlist(response_dts)
}


#' Get the fields of a Chembl resource
#'
#' This function retrieves the fields of a Chembl resource.
#'
#' @param resource The Chembl resource.
#' @return A character vector containing the names of the fields.
#'
#' @examples
#' getChemblResourceFields("molecule")
#'
#' @export
getChemblResourceFields <- function(resource) {
  checkmate::assert_choice(resource, .chembl_resources())
  .chembl_resource_schema(resource)[["fields"]] |> names()
}
