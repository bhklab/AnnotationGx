

.parseQueryToDT <- function(resp){
    data.table::as.data.table(resp[[1]][[1]])
}


getPubchemStatus <- function(
    returnMessage = FALSE, printMessage = TRUE,
    url = "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/Aspirin/cids/JSON"
    ){
    funContext <- .funContext("getPubchemStatus")

    request <- .buildURL(url) |> .build_pubchem_request()
    response <- httr2::req_perform(request)

    status_code <- httr2::resp_status(response)
    parsed_info <- .checkThrottlingStatus2(response, printMessage)
    if(returnMessage) return(parsed_info)
}


#' names are: request_count, request_time and service
#' each has status and percent
#' main throttlers for user are request_count and request_time
#' main statuses are:
#'  Green - less than 50% of the permitted request limit has been used
#'  Yellow - between 50% and 75% of the request limit has been used
#'  Red - more than 75% of the request limit has been reached
#'  Black - the limit has been exceeded and requests are being blocked
#' @keywords internal
.checkThrottlingStatus2 <- function(response, printMessage){
    message <- httr2::resp_headers(response)$`x-throttling-control`
    parsed_info <- .parse_throttling_message(message)
    if(printMessage){
        message("Throttling status:\n", paste0(strsplit(message, ", ")[[1]], collapse = "\n"))
    }
    # Check if the request count or request time is
    if(parsed_info$service$status == "Black"){
        message("WARNING: The request limit has been exceeded and requests are being blocked.")
    }else if(parsed_info$service$status %in% c("Red", "Yellow")){
        message("WARNING: The request limit has been reached or is close to being reached.")
    }else{
        message("The request limit is not close to being reached.")
    }
    return(parsed_info)
}


#' @keywords internal
.parse_throttling_message <- function(message) {
  # Split the message into components
  components <- strsplit(message, ", ")[[1]]
  
  # Initialize an empty list to store the parsed information
  parsed_info <- list()
  
  # Loop through each component and extract the relevant information
  for (comp in components) {
    # Split each component into key-value pairs
    kv <- strsplit(comp, ": ")[[1]]
    key <- tolower(gsub(" status", "", kv[1]))
    key <- gsub(" ", "_", key)
    value <- kv[2]
    
    # Extract status and percent
    status <- sub("\\s*\\(.*\\)", "", value)
    percent <- as.integer(sub(".*\\((\\d+)%\\).*", "\\1", value))
    
    # Store the extracted information in the parsed_info list
    parsed_info[[key]] <- list(status = status, percent = percent)
  }
  
  return(parsed_info)
}
