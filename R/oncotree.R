
#' Get data from Oncotree API
#'
#' This function retrieves data from the Oncotree API based on the specified target.
#'
#' @param target A character vector specifying the target data to retrieve. 
#'              Valid options are "versions", "mainTypes", and "tumorTypes".
#'
#' @return A data table containing the retrieved data.
#'
#' @noRd 
#' @keywords internal
.getRequestOncotree <- function(
    target = c("versions", "mainTypes", "tumorTypes")
) {
    
    url <- "http://oncotree.mskcc.org"
    targetClean <- match.arg(target)
    .buildURL(url, "api", targetClean) |> 
        .build_request() |>
        .perform_request() |>
        .parse_resp_json() |> 
        .asDT()
}
#' Get available Oncotree versions
#'
#' This function retrieves the available versions of Oncotree.
#'
#' @return A `data.table` containing available Oncotree versions.
#'
#' @export
getOncotreeVersions <- function() {
    .getRequestOncotree(target="versions")
}

#' Get the main types from the Oncotree database.
#'
#' This function retrieves the main types from the Oncotree database.
#' 
#' @return A `data.table` containing the main types from the Oncotree database.
#' 
#' @export
getOncotreeMainTypes <- function() {
    res <- .getRequestOncotree(target="mainTypes") 
    setnames(res, "mainType")
    return(res)
}


#' Get the tumor types from the Oncotree database.
#' 
#' This function retrieves the tumor types from the Oncotree database.
#' 
#' @return A `data.table` containing the tumor types from the Oncotree database.
#' 
#' @export
getOncotreeTumorTypes <- function() {
    .getRequestOncotree(target="tumorTypes")
}
