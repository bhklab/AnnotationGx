## =========================================
## Make GET Requests to the ChemBl REST API
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
    full_url <- (paste(url, compound_name))

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


# Write testing code here, this is only executed if the file is run as a script
# It is equivalent to if __name__ == "__main__" in Python
# What it actually does is count the number of stack frames
if (sys.nframe() == 0) {
    library(jsonlite)
    library(httr)
}