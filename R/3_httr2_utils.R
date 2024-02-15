#' Builds a URL by concatenating the input arguments and encoding it.
#'
#' @param ... The components of the URL.
#' @return The encoded URL.
#' @noRd
.buildURL <- function(...) {
    paste0(stats::na.omit(unlist(list(...))), collapse='/') |> utils::URLencode()
}

#' Builds an HTTP request using the provided URL.
#'
#' @param url The URL for the request.
#' @return The built HTTP request.
#' @noRd
.build_request <- function(url){
    httr2::request(url) |>
        httr2::req_retry(max_tries = 3) |>
        httr2::req_error(is_error = \(resp) FALSE)
}

#' Builds a PubChem HTTP request using the provided URL.
#'
#' @param url The URL for the request.
#' @return The built PubChem HTTP request.
#' @noRd
.build_pubchem_request <- function(url){
    httr2::request(url) |>
        httr2::req_retry(max_tries = 3) |>
        httr2::req_error(is_error = \(resp) FALSE)
}
.build_pubchem_request <- function(url){
    .build_request(url) |>
        httr2::req_throttle(rate = 1000/60)
}

#' Performs an HTTP request.
#'
#' @param request The HTTP request to perform.
#' @param parallel A logical value indicating whether to perform the request in parallel.
#' @return The response of the HTTP request.
.perform_request <- function(request, parallel=FALSE){
    httr2::req_perform(request)
}

#' Parses the JSON response from an HTTP request.
#'
#' @param resp The response object from the HTTP request.
#' @return The parsed JSON response.
.parse_resp_json <- function(resp){
    httr2::resp_body_json(resp, simplifyVector = TRUE)
}