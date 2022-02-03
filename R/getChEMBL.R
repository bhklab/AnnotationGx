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
        i <- i + 1
    }
    mechanism_df <- rbindlist(lapply(mechanism_list, FUN=as.data.table),
        fill=TRUE)
    return(mechanism_df)
}




# Write testing code here, this is only executed if the file is run as a script
# It is equivalent to if __name__ == "__main__" in Python
# What it actually does is count the number of stack frames
if (sys.nframe() == 0) {
    library(jsonlite)
    library(httr)
}