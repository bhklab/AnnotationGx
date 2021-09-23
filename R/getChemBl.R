## =========================================
## Make GET Requests to the ChemBl REST API
## -----------------------------------------



library(httr)
library(jsonlite)


getRequestChemBl <- function(compound_name) {
  # This is the url without the value in the key-value
  # pair for looking up CHEMBL_ID
  general = "https://www.ebi.ac.uk/chembl/api/data/chembl_id_lookup/search.json?q="
  
  # Generates a complete url taking the value from the parameter of the function
  # compound_name
  full_url <- (paste(general, compound_name))
  
  # Encoded the url to avoid html error
  encoded <- URLencode(full_url)
  
  # Parses the json formated data
  result <- parse_json(GET(encoded), as='text')
  
  # Returns the data along with the CHEMBL_ID, it should only return the CHEMBL_ID
  return(result$chembl_id)
}
  

