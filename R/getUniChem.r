## =========================================
## Make GET Requests to the UniChem REST API
## -----------------------------------------

#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
NULL

#' @description This function allows to map between external databases identifiers using the UniChem API.
#'  The UniChem database gathers its data from 48 different sources/databases. All sources are listed with their
#'  src_id and short name.
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
#' @param end_point A `character` vector which is the src_compound_id, the 
#'    id of that compound for that particular database/source
#' @param chemical_id A `character` vector which is the compound identifier for 
#'    the specified database/source
#'
#' @param src_name A `character` vector which is the short name for the database/
#'    source for which we know the chemical_id
#'
#' @param target_name A `character` vector which is the short name for the database/
#'    source for which we require the compounds end_point
#'
#' @return A `character` vector which is the src_compound_id for the target_name
#' 
#' 
#' 

library(httr)
library(jsonlite)

mapBetweenSources <- function(chemical_id, 
                              src_name, target_name, end_point="src_compound_id") {
  
  # A data frame storing all the short names of the sources/databases with the 
  # corresponding src_id
  name_to_id <- data.frame(row.names = c("chembl", "drugbank", "pdb", "gtopdb",
                                         "pubchem_dotf", "kegg_ligand", "chebi",
                                         "nih_ncc", "zinc", "emolecules", "ibm",
                                         "atlas", "fdasrs", "surechembl", "pharmgkb",
                                         "hmdb", "selleck", "pubchem_tpharma",
                                         "pubchem", "mcule", "nmrshiftdb2", "lincs",
                                         "actor", "recon", "molport", "nikkaji",
                                         "bindingDB", "comptox", "lipidmaps",
                                         "drugcentral", "carotenoiddb", "metabolights",
                                         "brenda", "rhea", "chemicalbook", "dailymed_old",
                                         "swiss_lipids", "dailymed", "clinicaltrials",
                                         "rxnorm",
                                         "MedChemExpress"),
                           val = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                                   "11", "12", "14", "15", "17", "18", "20", "21",
                                   "22", "23", "24", "25", "26", "27", "28", "29",
                                   "31", "32", "33", "34", "35", "36", "37", "38",
                                   "39", "40", "41", "45", "46", "47", "48"))
  
  # stores the src_id for the target_name database
  target_id <- name_to_id[target_name,]
  
  # stores the src_id for the src_name database
  src_id <- name_to_id[src_name,]
  
  # Base url for the UniChem REST API
  base_url <- "https://www.ebi.ac.uk/unichem/rest"
  
  #Generate the complete url for the get request
  result <- paste0(base_url, "/", end_point, "/", chemical_id, "/", src_id, "/",
                   target_id)
  #Encode the complete url
  encoded <- URLencode(result)
  
  #Make a get request to the UniChem REST API
  response <- GET(encoded)
  
  #Parse the json object to get an array
  final <- parse_json(response)
  
  return(final)
  
}

if (sys.nframe() == 0) {
  library(jsonlite)
  library(httr)
}
