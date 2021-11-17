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
#'  # TODO:: Convert this list into a markdown table (see https://www.tablesgenerator.com/markdown_tables)
#'  1 -> chembl
#'  2 -> drugbank
#'  3 -> pdb
#'  4 -> gtopdb
#'  5 -> pubchem_dotf
#'  6 -> kegg_ligand
#'  7 -> chebi
#'  8 -> nih_ncc
#'  9 -> zinc
#'  10 -> emolecules
#'  11 -> ibm
#'  12 -> atlas
#'  14 -> fdasrs
#'  15 -> surechembl
#'  17 -> pharmgkb
#'  18 -> hmdb
#'  20 -> selleck
#'  21 -> pubchem_tpharma
#'  22 -> pubchem
#'  23 -> mcule
#'  24 -> nmrshiftdb2
#'  25 -> lincs
#'  26 -> actor
#'  27 -> recon
#'  28 -> molport
#'  29 -> nikkaji
#'  31 -> bindingDB
#'  32 -> comptox
#'  33 -> lipidmaps
#'  34 -> drugcentral
#'  35 -> carotenoiddb
#'  36 -> metabolights
#'  37 -> brenda
#'  38 -> rhea 
#'  39 -> chemicalbook
#'  40 -> dailymed_old
#'  41 -> swiss_lipids
#'  45 -> dailymed
#'  46 -> clinicaltrials
#'  47 -> rxnorm
#'  48 -> MedChemExpress
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
  response <- GET(encoded)
  
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
  response <- GET(encoded)
  
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
identifierToInchikey <- function(chemical_id, src_id, ..., base_url="https://www.ebi.ac.uk/unichem/rest/structure/"){
  
  # Creates the url with the inchikey 
  final_url <- .buildURL(base_url, chemical_id, src_id)
  
  # Encodes the url 
  encoded <- URLencode(final_url)
  
  # Makes a GET request to the UniChem API
  response <- GET(encoded)
  
  # Parse json object 
  final <- parse_json(response)

  return(final)
}
  

## TODO:: Determine if this function specification is correct, update as needed to work with the UniChem API
#' 
#' @param inchi A `character(1)` vector containing the InchiKey if `type`="key"
#' @param target_name A `character(1)` vector with the name of the database
#' to return the identifier for. Defaults to UniChem.
#'
#' @return 
#'
#' @export
inchiToDatabaseID <- function(inchi, target_name,..., type=c("key", "structure")) {

  dbname_to_id <- .getDatabaseNameToUniChemID()

  target_id <- tryCatch({
    dbname_to_id[[target_name]]
  }, error=function(e) stop("The `target_name` look-up failed. Please ensure
    the target database name is spelled correctly and matches the UniChem API
    specifications: \n", e)
  )

  # build the API query

  # parse to a data.frame

  # return to the user
}

## TODO:: After this structure based query is done, we need to write wrapper
## functions which do look ups from a vector of database IDs or InchiKeys
## This should be parallelized using BiocParallel, probably with bplapply\

## Once the function is working, we may need to use the ProxyManager class
## to proxy massively parallel API queries, this prevents IP blocking when
## too many requests originate from a single IP in a short amount of time
## This will depend on whether or not they enforce any query speed limits for
## the Unichem API, please consult the API documentation to find out


if (sys.nframe() == 0) {
  library(jsonlite)
  library(httr)

}
