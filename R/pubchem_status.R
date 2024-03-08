
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
    parsed_info <- .checkThrottlingStatus2(response, printMessage)
    if(returnMessage) return(parsed_info)
}
