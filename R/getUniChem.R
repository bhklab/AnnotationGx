## =========================================
## Make GET Requests to the UniChem REST API
## -----------------------------------------

#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
NULL

#' Returns a list mapping from database name to database ID for the UniChem API
#'
#' @description
#' For mapping between the name of a database and its integer indentifier when
#' making API queries to the EMBL UniChem API
#'
#' @return A `list` where names are shortened database names used in UniChem and
#' values are the associated integer identifier in the UniChem database
#'
#' @noRd
.getDatabaseNameToUniChemID <- function() {
  list(chembl = "1", drugbank = "2", pdb = "3", gtopdb = "4", pubchem_dotf = "5",
    kegg_ligand = "6", chebi = "7", nih_ncc = "8", zinc = "9", emolecules = "10",
    ibm = "11", atlas = "12", fdasrs = "14", surechembl = "15", pharmgkb = "17",
    hmdb = "18", selleck = "20", pubchem_tpharma = "21", pubchem = "22",
    mcule = "23", nmrshiftdb2 = "24", lincs = "25", actor = "26", recon = "27",
    molport = "28", nikkaji = "29", bindingDB = "31", comptox = "32",
    lipidmaps = "33", drugcentral = "34", carotenoiddb = "35", metabolights = "36",
    brenda = "37", rhea = "38", chemicalbook = "39", dailymed_old = "40",
    swiss_lipids = "41", dailymed = "45", clinicaltrials = "46", rxnorm = "47",
    MedChemExpress = "48")
}

#' @description This function allows to map between external databases identifiers using the UniChem API.
#'  The UniChem database gathers its data from 48 different sources/databases. All sources are listed with their
#'  src_id and short name.
#'
#'  |src#| src_name        |
#'  | 1  | chembl          |
#'  | 2  | drugbank        |
#'  | 3  | pdb             |
#'  | 4  | gtopdb          |
#'  | 5  | pubchem_dotf    |
#'  | 6  | kegg_ligand     |
#'  | 7  | chebi           |
#'  | 8  | nih_ncc         |
#'  | 9  | zinc            |
#'  | 10 | emolecules      |
#'  | 11 | ibm             |
#'  | 12 | atlas           |
#'  | 14 | fdasrs          |
#'  | 15 | surechembl      |
#'  | 17 | pharmakb        |
#'  | 18 | hmdb            |
#'  | 20 | selleck         |
#'  | 21 | pubchem_tpharma |
#'  | 22 | pubchem         |
#'  | 23 | mcule           |
#'  | 24 | nmrshiftdb2     |
#'  | 25 | lincs           |
#'  | 26 | actor           |
#'  | 27 | recon           |
#'  | 28 | molport         |
#'  | 29 | nikkaji         |
#'  | 31 | bindingDB       |
#'  | 32 | comptox         |
#'  | 33 | lipidmaps       |
#'  | 34 | drugcentral     |
#'  | 35 | carotenoiddb    |
#'  | 36 | metabolights    |
#'  | 37 | brenda          |
#'  | 38 | rhea            |
#'  | 39 | chemicalbook    |
#'  | 40 | dailymed_old    |
#'  | 41 | swiss_lipids    |
#'  | 45 | dailymed        |
#'  | 46 | clinicaltrials  |
#'  | 47 | rxnorm          |
#'  | 48 | MedChemExpress  |
#'
#' @param chemical_id A `character` vector which is the compound identifier for
#'    the specified database/source
#' @param src_name A `character` vector which is the short name for the database/
#'    source for which we know the chemical_id
#' @param target_name A `character` vector which is the short name for the database/
#'    source for which we require the compounds end_point
#' @param ... Force subsequent parameters to be named. Not used.
#' @param end_point A `character` vector which is the src_compound_id, the
#'    id of that compound for that particular database/source
#' @param base_url A `character` vector specifying the base_url to use when constructing
#'    API queries. This is for developer use.
#'
#' @return A `character` vector which is the src_compound_id for the target_name
#'
#' @export
mapBetweenSources <- function(chemical_id, src_name, target_name, ...,
                              end_point="src_compound_id",
                              base_url="https://www.ebi.ac.uk/unichem/rest") {

  # A list mapping from data source names to their associated UniChem source ids
  name_to_id <- .getDatabaseNameToUniChemID()

  # stores the src_id for the target_name database
  target_id <- name_to_id[[target_name]]

  # stores the src_id for the src_name database
  src_id <- name_to_id[[src_name]]

  #Generate the complete url for the get request
  result <- .buildURL(base_url, end_point, chemical_id, src_id, target_id)
  #Encode the complete url
  encoded <- URLencode(result)

  #Make a get request to the UniChem REST API
  response <- httr::RETRY("GET", encoded, timeout(29), times=3, quite=TRUE,
                          terminate_on = c(400, 404, 503))

  #Parse the json object to get an array
  final <- parse_json(response)

  return(final)
}

#' @description This function takes in inchikey as a parameter and allows to map
#' to any identifier using the UniChem API
#'
#' @param inchikey A `character` vector which is unique to each compound.
#'
#' @return A data.frame with source id and source compound id.
#'
#' @export
inchiKeyToIdentifier <- function(inchi_key, ..., base_url ="https://www.ebi.ac.uk/unichem/rest/inchikey/"){

  # Creates the url with the inchikey
  final_url <- .buildURL(base_url, inchi_key)

  # Encodes the url
  encoded <- URLencode(final_url)

  # Makes a GET request to the UniChem API
  response <- httr::RETRY("GET", encoded, timeout(29), times=3, quite=TRUE,
                          terminate_on = c(400, 404, 503))

  # Parse json object
  result <- parse_json(response)

  # Convert to a dataframe
  final <- as.data.frame(result)

  return(final)
}


#' @description This function takes in chemical id and source id and returns the
#' inchi and inchikey structure
#'
#' @param chemical_id A `character` vector which is the src_compound_id
#'
#' @param src_id A `character` vector which is the id for the database/source
#'
#' @return A `character` vector with the inchikey and inchi structure
#'
#' @export
identifierToInchikey <- function(chemical_id, target_names, ...,
                                 base_url="https://www.ebi.ac.uk/unichem/rest/structure/", inchikey=TRUE){

  dbname_to_id <- .getDatabaseNameToUniChemID()

  sr_id <- dbname_to_id[[target_names]]

  # Creates the url with the inchikey
  final_url <- .buildURL(base_url, chemical_id, sr_id)

  # Encodes the url
  encoded <- URLencode(final_url)

  # Makes a GET request to the UniChem API
  response <- httr::RETRY("GET", encoded, timeout(29), times=3, quite=TRUE,
                          terminate_on = c(400, 404, 503))

  # Parse json object
  final <- parseJSON(response)

  if (inchikey == TRUE){
    return(final$standardinchikey)
  }else{
    return(final$standardinchi)
  }
}

## TODO:: Determine if this function specification is correct, update as needed to work with the UniChem API
#'
#' @param inchi A `character(1)` vector containing the InchiKey if `type`="key"
#' @param target_names A `character()` vector specifying the name of the target
#'   database id to return. If missing, returns all
#'
#' @return A `data.frame` mapping from the inchi or inchikey to the
#'
#' @importFrom httr GET
#' @export
inchiToDatabaseID <- function(inchi, target_names, ..., type=c("key", "structure",),
                              base_url="https://www.ebi.ac.uk/unichem/rest") {

  ## TODO:: Maybe it is better to make .getDatabaseNameToUniChemID return a data.frame?
  dbname_to_id <- .getDatabaseNameToUniChemID()
  dbname_df <- data.frame(src_id=unlist(dbname_to_id),
                          database_id=names(dbname_to_id))


  if (!missing(target_names)) {
    valid_target_names <- target_names %in% dbname_df$database_id
    if (!all(valid_target_names)) {
      stop("One or more specificed target databases do not exist: ",

        paste0(target_names[!valid_target_names], collapse=", "))
    }
  }

  # build the API query
  query <- .buildURL(base_url, "inchikey", inchi)
  encoded <- URLencode(query)
  response <- httr::RETRY("GET", encoded, timeout(29), times=3, quite=TRUE,
                          terminate_on = c(400, 404, 503))

  # Parse to a data frame
  result <- parseJSON(response)
  # Merge does a SQL style left join between two data.frames on the specified
  #  by shared column (see ?merge for more details)
  result <- merge(dbname_df, result, by="src_id")

  if (!missing(target_names)) {
    # Not using rownames here incase the input is a data.table, which never
    #  has rownames
    result <- result[result$database_id %in% target_names, ]
  }

  # return to the user
  return(result)
}

## TODO:: After this structure based query is done, we need to write wrapper
## functions which do look ups from a vector of database IDs or InchiKeys
## This should be parallelized using BiocParallel, probably with bplapply

wInchiToDatabaseID <- function(inchis, target_names) {
  result <- bplapply(X = inchis, FUN = inchiToDatabaseID, target_names=target_names)
  return(result)
}

wIdentifierToInchiKey <- function(chemical_ids, target_names) {
  result <- bplapply(X = chemical_ids, FUN = identifierToInchikey, target_names=target_names)
  return(result)
}

wMapBetweenSources <- function(chemical_ids, src_name, target_name) {
  result <- bplapply(X = chemical_ids, FUN = mapBetweenSources, src_name=src_name, target_name=target_name)
  return(result)
}


## Once the function is working, we may need to use the ProxyManager class
## to proxy massively parallel API queries, this prevents IP blocking when
## too many requests originate from a single IP in a short amount of time
## This will depend on whether or not they enforce any query speed limits for
## the Unichem API, please consult the API documentation to find out


if (sys.nframe() == 0) {
  # Source utility functions
  source("R/utils.R")
  source("R/parseJSON.R")

  # Load required libraries
  library(jsonlite)
  library(httr)
  library(BiocParallel)

  # Example code
  inchi <- "AAKJLRGGTJKAMG-UHFFFAOYSA-N"
  target_names <- c("chembl", "pubchem")
  type <- "key"
  ve <- c("AAKJLRGGTJKAMG-UHFFFAOYSA-N", "BCFGMOOMADDAQU-UHFFFAOYSA-N")
  ve2 <- c("CHEMBL12", "CHEMBL11")
  ve3 <- c("CHEMBL12", "CHEMBL11")
  # Specified target names
  database_specific_ids <- inchiToDatabaseID(inchi=inchi,
    target_names=target_names)

  # All database identifiers
  database_ids <- inchiToDatabaseID(inchi=inchi)
  test <- identifierToInchikey("CHEMBL12", "chembl")
  test2 <- wInchiToDatabaseID(ve, target_names = target_names)
  test3 <- wIdentifierToInchiKey(ve2, target_names = "chembl")
  test4 <- wMapBetweenSources(ve3, src_name="chembl", target_name="pubchem")
  test5 <- identifierToInchikey("CHEMBL12", "chembl")
  test6 <- mapBetweenSources("CHEMBL12", "chembl", "pubchem")
}
