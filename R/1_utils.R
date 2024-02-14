
#' simple wrapper for the data.table::as.data.table() function
#' @param x object to convert to a data.table
#' @param ... additional arguments to pass to data.table::as.data.table()
#' @return a data.table
#' @keywords internal
#' @noRd
.asDT <- function(x, ...) data.table::as.data.table(x, ...)


.buildURL <- function(...) {
    paste0(stats::na.omit(unlist(list(...))), collapse='/') |> utils::URLencode()
}

.build_request <- function(url){
    httr2::request(url) |>
        httr2::req_retry(max_tries = 3) |>
        httr2::req_error(is_error = \(resp) FALSE)
}

.build_pubchem_request <- function(url){
    .build_request(url) |>
        httr2::req_throttle(rate = 1000/60)
}

.perform_request <- function(request, parallel=FALSE){
    httr2::req_perform(request)
}



.parse_resp_json <- function(resp){
    httr2::resp_body_json(resp, simplifyVector = TRUE)
}