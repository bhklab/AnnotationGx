
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
    
    if(response$response != "Success"){
        .err(funContext, "Unichem API request failed.")
    }

    .debug(funContext, sprintf("Unichem sourceCount: %s", response$totalSources))

    sources_dt <- .asDT(response$sources)

    old_names <- c(
        "UCICount", "baseIdUrl", "description", "lastUpdated", "name", 
        "nameLabel", "nameLong", "sourceID", "srcDetails", "srcReleaseDate", 
        "srcReleaseNumber", "srcUrl", "updateComments")

    new_names <- c(
        "CompoundCount", "BaseURL", "Description", "LastUpdated", "Name", 
        "NameLabel", "NameLong", "SourceID", "Details", "ReleaseDate",
        "ReleaseNumber", "URL", "UpdateComments")
    
    setnames(sources_dt, old_names, new_names)

    new_order <- c(
        "Name", "NameLabel", "NameLong", "SourceID", "CompoundCount", 
        "BaseURL", "URL", "Details",
        "Description", "ReleaseNumber", "ReleaseDate", "LastUpdated", 
        "UpdateComments"
    )


    sources_dt <- sources_dt[, new_order, with = FALSE]

    if(all_columns) return(sources_dt)

    sources_dt[, c("Name", "SourceID")]

}

#' Query UniChem for a compound.
#' 
#' This function queries the UniChem API for a compound based on the provided parameters.
#' 
#' @param compound `character` or `integer` The compound identifier to search for.
#' @param type `character` The type of compound identifier to search for. Valid types are "uci", "inchi", "inchikey", and "sourceID".
#' @param sourceID `integer` The source ID to search for if the type is "sourceID". Defaults to NULL.
#' @param request_only `boolean` Whether to return the request only. Defaults to FALSE.
#' @param raw `boolean` Whether to return the raw response. Defaults to FALSE.
#' @param ... Additional arguments.
#' 
#' @return A list with the external mappings and the UniChem mappings.
#' 
#' @examples
#' queryUnichemCompound(type = "sourceID", compound = "444795", sourceID = 22)
#' 
#' @export
queryUnichemCompound <- function(
    compound, type, sourceID = NA_integer_, request_only = FALSE, raw = FALSE, ...
){
    checkmate::assert_string(type)
    checkmate::assert_atomic(compound)
    checkmate::assert_integerish(sourceID)
    checkmate::assertLogical(request_only)
    checkmate::assertLogical(raw)

    request <- .build_unichem_compound_req(type, compound, sourceID,...)
    if(request_only) return(request)

    response <- request |> 
        .perform_request() |>  
        .parse_resp_json() 
    
    if(raw) return(response)

    if(response$response != "Success"){
        .err("Unichem API request failed.")
    }

    # Mapping names to be consistent with other API calls
    mapped_sources_dt <- .asDT(response$compounds$sources)
    old_names <- c("compoundId", "shortName", "longName", "id", "url")

    new_names <- c("compoundID", "Name", "NameLong", "sourceID", "sourceURL")
    data.table::setnames(mapped_sources_dt, old = old_names, new = new_names)

    External_Mappings <- mapped_sources_dt[, new_names, with = FALSE]
    
    UniChem_Mappings <- list(
        UniChem.UCI = response$compounds$uci,
        UniChem.InchiKey = response$compounds$standardInchiKey,
        UniChem.Inchi = response$compounds$inchi$inchi,
        UniChem.formula = response$compounds$inchi$formula,
        UniChem.connections = response$compounds$inchi$connections,
        UniChem.hAtoms = response$compounds$inchi$hAtoms
    )

    list(
        External_Mappings = External_Mappings,
        UniChem_Mappings = UniChem_Mappings
    )

}