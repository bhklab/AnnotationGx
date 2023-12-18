# #' Generic GET requests to the Oncotree API
# #'
# #' @param ... `pairlist` Fall through arguments to `httr::GET`
# #' @param url `character(1)` The Oncotree API URL. Do not change, just for
# #'   debugging.
# #' @param target `character(1)`
# #'
# #' @importFrom httr RETRY GET
# #' @importFrom data.table as.data.table data.table
# .getRequestOncotree <- function(
#         target=c("versions", "mainTypes", "tumorTypes"),
#         ..., url="http://oncotree.mskcc.org") {
#     stopifnot(is.character(url) && length(url) == 1)
#     targetClean <- match.arg(target)
#     queryUrl <- .buildURL(url, "api", targetClean)
#     response <- httr::RETRY("GET", queryUrl, ...)
#     return(response)
# }

# #' Retrieve a table of Oncotree release versions
# #'
# #' @param ... `pairlist` Fall through arguments to `httr::GET`
# #'
# #' @return `data.frame` Table of metadata for all Oncotree release versions
# getOncotreeVersions <- function(...) {
#     res <- .getRequestOncotree(target="versions")
#     oncotreeVersions <- parseJSON(res)
#     return(oncotreeVersions)
# }

# #' Retrieve the main tumor types annotations from Oncotree
# #'
# #' @param ... `pairlist` Fall through arguments to `httr::GET`
# #'
# #' @return `character` Vector of main tumour types from Oncotree.
# getOncotreeMainTypes <- function(...) {
#     res <- .getRequestOncotree(target="mainTypes", ...)
#     oncotreeTypes <- parseJSON(res)
#     return(oncotreeTypes)
# }

# #' Retrieve the main tissue type annotations from Oncotree
# #'
# #' @param ... `pairlist` Fall through arguments to `httr::GET`
# #'
# #' @return `data.frame` Table of Oncotree tumour types, including
# #'   tissue types, source and other metadata.
# getOncotreeTumorTypes <- function(tissue, query, ...) {
#     if (!missing(tissue) || !missing(query))
#         .warning("Specific queries are not yet implemented! Ignorning these parameters")
#     res <- .getRequestOncotree(target="tumorTypes")
#     oncotreeTumor <- parseJSON(res)
#     return(oncotreeTumor)
# }

# # Only run if file called as script
# if (sys.nframe() == 0) {
#     library(httr)
#     library(data.table)
#     library(jsonlite)
#     source("R/utils.R")
#     source("R/parseJSON.R")

#     # 1
#     res <- .getRequestOncotree()
#     df_ <- parseJSON(res)
# }