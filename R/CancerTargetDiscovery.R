# CancerTargetDiscoveryDevelopment Functions

#' Map Compound to CTD
#'
#' This function maps a drug compound to the Cancer Target Discovery (CTD) database.
#' It retrieves information about the compound from the CTD database and returns the results as a data table.
#'
#' @param compounds A character vector of drug compounds to map to the CTD database.
#' @param base_url The base URL of the CTD API. Default is "https://ctd2-dashboard.nci.nih.gov/dashboard/get".
#' @param endpoint The API endpoint for the compound mapping. Default is "compound".
#' @param nParallel The number of parallel processes to use. Default is one less than the number of available cores.
#' @param raw Logical indicating whether to return the raw response from the API. Default is FALSE.
#' @param query_only Logical indicating whether to only return the API request URL without making the actual request. Default is FALSE.
#'
#' @return A data table containing the mapped information for the drug compound.
#' If the API request fails, a data table with the drug compound name will be returned.
#' If \code{raw} is set to TRUE, the raw response from the API will be returned.
#'
#' @examples
#' mapCompound2CTD("Bax channel blocker", nParallel = 1)
#'
#' @export
mapCompound2CTD <- function(
    compounds,
    base_url = "https://ctd2-dashboard.nci.nih.gov/dashboard/get", 
    endpoint = "compound",
    nParallel = parallel::detectCores() - 1,
    raw = FALSE,
    query_only = FALSE
) {
    funContext <- .funContext("mapCompound2CTD")
    
    # Check input types
    checkmate::assert_character(compounds)
    checkmate::assert_character(base_url)
    checkmate::assert_character(endpoint)
    checkmate::assert_logical(raw)
    checkmate::assert_logical(query_only)

    .info(funContext, 
        sprintf("Creating requests for %s compounds", length(compounds)))

    requests <- parallel::mclapply(compounds, function(compound){
        compound <- gsub(" ", "-", compound)

        .buildURL(base_url, endpoint, compound) |>
            .build_request()
    })
    if(query_only) return(requests)

    .info(funContext, "Performing requests w/", nParallel, "parallel processes..")
    resps <- .perform_request_parallel(requests)
    names(resps) <- compounds

    results <- parallel::mclapply(
        names(resps) , 
        function(compound){
            resp <- resps[[compound]]

            if(all(class(resp) != "httr2_response") || resp$status_code != 200){
                dt <- data.table::data.table(
                    displayName = compound)
                return(dt)
            }

            resp <- .parse_resp_json(resp)
            if(raw) return(resp)

            original_dt <- .asDT(resp$xrefs)[, c("databaseId", "databaseName")]
            original_dt[, "displayName" := resp$displayName]

            dt <- data.table::dcast(
                    original_dt, 
                    formula = displayName ~ databaseName, 
                    value.var = "databaseId"
                )
            return(dt)
        }, 
        mc.cores = nParallel    
    )
    if(raw) {
        names(results) <- compounds
        return(results)
    }

    return(data.table::rbindlist(results, fill = TRUE))
}

