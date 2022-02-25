## Make GET Requests to the ChemBl REST API
## =========================================
## -----------------------------------------

#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
NULL


#'
getRequestChembl <- function(compound_name,
        url="https://www.ebi.ac.uk/chembl/api/data/chembl_id_lookup/search.json?q=") {
    # This is the url without the value in the key-value
    # pair for looking up CHEMBL_ID
    #general

    # Generates a complete url taking the value from the parameter of the function
    # compound_name
    full_url <- paste0(url, compound_name)

    # Encoded the url to avoid html error
    encoded <- URLencode(full_url)

    # Parses the json formated data
    result <- parseJSON(GET(encoded))

    # Returns the data along with the CHEMBL_ID, it should only return the CHEMBL_ID
    return(result$chembl_id)
}

#'
getChemblMolecule <- function(molecule_name, raw=FALSE,
        url="https://www.ebi.ac.uk/chembl/api/data/molecule/search.json?q=") {
    query_url <- paste0(url, molecule_name)
    encoded_query_url <- URLencode(query_url)

    response <- GET(encoded_query_url)

    if (raw) return(response)

    response_list <- parseJSON(response)

    ## TODO: Unnest the data.frame
    return(response_list)
}

#' Retrieve a `data.table` of all molecule mechanisms of action from ChEMBL
#'
#' @param url `character(1)` Base URL of ChEMBL API.
#' @param query `character(1)` ChEMBL API query.
#'
#' @return `data.table` A table containing all mechanism of action entries
#'   availble in ChEMBL.
#'
#' @importFrom httr GET status_code
#' @importFrom data.table as.data.table rbindlist
#' @export
getChemblAllMechanisms <- function(url="https://www.ebi.ac.uk",
        query="/chembl/api/data/mechanism.json?limit=1000") {
    mechanism_list <- list()
    i <- 1
    while (is.character(query)) {
        response <- GET(URLencode(paste0(url, query)))
        if (status_code(response) != 200) {
            warning("Query failed")
            return(mechanism_list)
        }
        response_list <- parseJSON(response)
        mechanism_list[[i]] <- response_list$mechanisms
        query <- response_list$page_meta$`next`
        print(query)
        i <- i + 1
    }
    mechanism_df <- rbindlist(lapply(mechanism_list, FUN=as.data.table),
        fill=TRUE)
    return(mechanism_df)
}

#'
#' @description A general function for creating Queries to the ChEMBL API 
#'
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
#'|        Filter Type       |                                                  Description                                                 |
#'|:------------------------:|:------------------------------------------------------------------------------------------------------------:|
#'| exact (iexact)           | Exact match with query (case insensitive equivalent)                                                         |
#'| contains (icontains)     | Wild card search with query (case insensitive equivalent)                                                    |
#'| startswith (istartswith) | Starts with query (case insensitive equivalent)                                                              |
#'| endswith (iendswith)     | Ends with query (case insensitive equivalent)                                                                |
#'| regex (iregex)           | Regular expression query (case insensitive equivalent)                                                       |
#'| gt (gte)                 | Greater than (or equal)                                                                                      |
#'| lt (lte)                 | Less than (or equal)                                                                                         |
#'| range                    | Within a range of values                                                                                     |
#'| in                       | Appears within list of query values                                                                          |
#'| isnull                   | Field is null                                                                                                |
#'| search                   | Special type of filter allowing a full text search based on elastic search queries                           |
#'| only                     | Select specific properties from the original endpoint and returns only the desired properties on each record |                                                                                                         |
#'
#'
constructChemblQuery <- function(resource, field, filter_type, value){
    
    url <- "https://www.ebi.ac.uk/chembl/api/data/"
    
    final <- paste0(url, resource, ".json", "?", field, "__", filter_type, "=", value)
    
    return (final)
}

#' 
#' @description A specialised function for querying the compund_record resource
#' from the ChEMBL API
#' 
#'    |       Field        |
#'    |--------------------|
#'    | compound_key       |
#'    | compound_name      |
#'    | document_chembl_id |
#'    | record_id          |
#'    | src_id             |
#'    
compoundQuery <- function(field, filter_type, value){
    url <- constructChemblQuery("compound_record.json", field, filter_type, value)
    url <- URLencode(url)
    
    response <- GET(url)
    response <- parse_json(response)
    return (response)
    
}

#'
#'
#' |         Field        |                                                                                                                                        Subfields                                                                                                                                       |
#' |:--------------------:|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|
#' | atc_classifications  |                                                                                                                                                                                                                                                                                        |
#' | availability_type    |                                                                                                                                                                                                                                                                                        |
#' | biotherapeutic       |                                                                                                                                                                                                                                                                                        |
#' | black_box_warning    |                                                                                                                                                                                                                                                                                        |
#' | chebi_par_id         |                                                                                                                                                                                                                                                                                        |
#' | chirality            |                                                                                                                                                                                                                                                                                        |
#' | cross_references     |                                                                                                                                                                                                                                                                                        |
#' | dosed_ingredient     |                                                                                                                                                                                                                                                                                        |
#' | first_approval       |                                                                                                                                                                                                                                                                                        |
#' | first_in_class       |                                                                                                                                                                                                                                                                                        |
#' | helm_notation        |                                                                                                                                                                                                                                                                                        |
#' | indication_class     |                                                                                                                                                                                                                                                                                        |
#' | inorganic_flag       |                                                                                                                                                                                                                                                                                        |
#' | max_phase            |                                                                                                                                                                                                                                                                                        |
#' | molecule_chembl_id   |                                                                                                                                                                                                                                                                                        |
#' | molecule_hierarchy   | molecule_chembl_id, parent_chembl_id,                                                                                                                                                                                                                                                  |
#' | molecule_properties  | alogp, aromatic_rings, cx_logd, cx_logp, cx_most_apka, cx_most_bpka, full_molformula, full_mwt, hba, hba_lipinski, hbd, hbd_lipinski, heavy_atoms, molecular_species, mw_freebase, mw_monoisotopic, num_lipinski_ro5_violations, num_ro5_violations, psa, qed_weighted, ro3_pass, rtb  |
#' | molecule_structures  | canonical_smiles, molfile, standard_inchi, standard_inchi_key                                                                                                                                                                                                                          |
#' | molecule_synonyms    |                                                                                                                                                                                                                                                                                        |
#' | molecule_type        |                                                                                                                                                                                                                                                                                        |
#' | natural_product      |                                                                                                                                                                                                                                                                                        |
#' | oral                 |                                                                                                                                                                                                                                                                                        |
#' | parenteral           |                                                                                                                                                                                                                                                                                        |
#' | polymer_flag         |                                                                                                                                                                                                                                                                                        |
#' | pref_name            |                                                                                                                                                                                                                                                                                        |
#' | pro_drug             |                                                                                                                                                                                                                                                                                        |
#' | structure_type       |                                                                                                                                                                                                                                                                                        |
#' | therapeutic_flag     |                                                                                                                                                                                                                                                                                        |
#' | topical              |                                                                                                                                                                                                                                                                                        |
#' | usan_stem            |                                                                                                                                                                                                                                                                                        |
#' | usan_stem_definition |                                                                                                                                                                                                                                                                                        |
#'| usan_substem          |                                                                                                                                                                                                                                                                                        |
#'| usan_year             |                                                                                                                                                                                                                                                                                        |
#' | withdrawn_class      |                                                                                                                                                                                                                                                                                        |
#' | withdrawn_country    |                                                                                                                                                                                                                                                                                        |
#' | withdrawn_flag       |                                                                                                                                                                                                                                                                                        |
#' | withdrawn_reason     |                                                                                                                                                                                                                                                                                        |
#' | withdrawn_year       |                                                                                                                                                                                                                                                                                        |                                                                                                                                                                                                                                                                                   |
#'
#'
moleculeQuery <- function(field, filter_type, value){
    url <- constructChemblQuery("molecule.json", field, filter_type, value)
    url <- URLencode(url)
    
    response <- GET(url)
    response <- parse_json(response)
    return (response)
}




# Write testing code here, this is only executed if the file is run as a script
# It is equivalent to if __name__ == "__main__" in Python
# What it actually does is count the number of stack frames
if (sys.nframe() == 0) {
    library(jsonlite)
    library(httr)
    test1 <- constructChemblQuery("target", "pref_name", "contains", "kinase")
    test2 <- compoundQuery("field", "fil", "j")
}