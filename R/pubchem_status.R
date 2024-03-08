
#' Retrieves the status of a PubChem request
#'
#' This function sends a request to PubChem to retrieve the status of a given URL.
#' It returns the status code and, if specified, the parsed information from the response.
#'
#' @param returnMessage Logical indicating whether to return the parsed information from the response.
#' @param printMessage Logical indicating whether to print the status message.
#' @param url The URL to send the request to. Default is "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/Aspirin/cids/JSON".
#'
#' @return The status code of the response. If \code{returnMessage} is \code{TRUE}, the parsed information from the response is also returned.
#'
#' @examples
#' getPubchemStatus()
#' getPubchemStatus(returnMessage = TRUE)
#' getPubchemStatus(printMessage = FALSE)
#'
#' @export
getPubchemStatus <- function(
    returnMessage = FALSE, printMessage = TRUE,
    url = "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/Aspirin/cids/JSON"
    ){
    funContext <- .funContext("getPubchemStatus")

    request <- .buildURL(url) |> .build_pubchem_request()
    response <- httr2::req_perform(request)

    status_code <- httr2::resp_status(response)
    message <- response$headers[["X-Throttling-Control"]]
    parsed_info <- .checkThrottlingStatus2(message, printMessage)
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
#' 
#' @noRd
#' @keywords internal
.checkThrottlingStatus2 <- function(message, printMessage){
    
    parsed_info <- .parse_throttling_message(message)
    if(printMessage){
        message("Throttling status:\n", paste0(strsplit(message, ", ")[[1]], collapse = "\n"))
    }
    # Check if the request count or request time is
    if(parsed_info$service$status == "Black"){
        .warn("The request limit has been exceeded and requests are being blocked.")
    }else if(parsed_info$service$status %in% c("Red", "Yellow")){
        .warn("The request limit has been reached or is close to being reached.")
    }else{
        .debug("The request limit is not close to being reached.")
    }
    return(parsed_info)
}

#' Parses the throttling message from the response
#' @noRd
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