#' Checks the throttling status of a request and sleeps if necessary
#'
#' This function checks the throttling status of a request by analyzing the
#' "x-throttling-control" header in the response. If the throttling status
#' indicates that the request is being throttled, the function sleeps for a
#' certain amount of time based on the percentage of throttling. The sleep time
#' is determined as follows:
#' - If the percentage of throttling is greater than 50%, the function sleeps
#'   for 30 seconds.
#' - If the percentage of throttling is greater than 30%, the function sleeps
#'   for 20 seconds.
#' - If the percentage of throttling is greater than 15%, the function sleeps
#'   for 15 seconds.
#' - Otherwise, the function sleeps for the same amount of time as the
#'   percentage of throttling.
#'
#' @param result The result of the request
#' @param throttleMessage Logical indicating whether to print the throttling
#'   message
#' @return Logical indicating whether the request was throttled
.checkThrottlingStatus <- function(result, throttleMessage = FALSE){
    message <- headers(result)$`x-throttling-control`

    if (throttleMessage == TRUE){
        message(message)
    }

    matches <- regmatches(message, gregexpr("\\((.*?)%\\)", message))
    percentages <- as.numeric(gsub("\\(|%|\\)", "", unlist(matches[1:3])))
    percentage <- max(percentages[1:3])
    sleep_time <- ifelse(percentage > 50, 30, ifelse(percentage > 30, 20, 15))

    if (percentage > 15) {
        Sys.sleep(sleep_time)
    } else {
        Sys.sleep(percentage)
    }

    return(percentage > 15)
}

#' simple checkThrottling Status by a default response 
#' 
#' This function checks the throttling status of a PubChem query and returns 
#' a logical value indicating whether the query is throttled.
#' 
#' @param sleep A logical value indicating whether to sleep before checking 
#' the throttling status.
#' @return A logical value indicating whether the query is throttled.
#' @export
#' 
#' @examples
#' getPubChemStatus()
#' getPubChemStatus(sleep = TRUE)
#' 
#' @keywords internal
getPubChemStatus <- function(sleep = FALSE){
    response <- httr::GET(
        "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/Aspirin/cids/JSON")
    msg <- headers(response)$`x-throttling-control`
    # Extracts text within parentheses
    matches <- regmatches(msg, gregexpr("\\((.*?)%\\)", msg))  
    if(sleep) .checkThrottlingStatus(response)
    message(msg)
}


# # -----------------------------
# # getPubChemAnnotations Helpers

# #' @importFrom data.table data.table as.data.table merge.data.table last rbindlist
# .parseATCannotations <- function(DT) {
#     dataL <- DT$Data
#     names(dataL) <- DT$SourceID
#     dataDT <- rbindlist(dataL, fill=TRUE, use.names=TRUE, idcol='SourceID')
#     dataDT[, ATC_code := unlist(lapply(Value.StringWithMarkup,
#         function(x) last(x)[[1]]))]
#     annotationDT <- merge.data.table(
#         dataDT[, .(SourceID, ATC_code)],
#         DT[, .(SourceName, SourceID, LinkedRecords)],
#         by='SourceID',
#         allow.cartesian=TRUE
#     )
#     DT <- annotationDT[, .(CID=unlist(LinkedRecords)),
#         by=.(SourceName, SourceID, ATC_code)]
#     DT <- unique(DT)
#     return(DT)
# }

# #' @importFrom data.table data.table as.data.table merge.data.table last rbindlist
# .parseDILIannotations <- function(DT) {
#     dataL <- DT$Data
#     names(dataL) <- DT$SourceID
#     dataL <- lapply(dataL, FUN=`[`, i=Name %like% 'DILI')
#     dataDT <- rbindlist(dataL, fill=TRUE, use.names=TRUE, idcol='SourceID')
#     dataDT[, DILI := unlist(Value.StringWithMarkup)]
#     annotationDT <- merge.data.table(
#         dataDT[, .(SourceID, DILI)],
#         DT[, .(SourceID, SourceName, Name, LinkedRecords.CID,
#             LinkedRecords.SID)],
#         by='SourceID',
#         allow.cartesian=TRUE)
#     DT <- 
#     annotationDT[, 
#         .(CID=unlist(LinkedRecords.CID), SID=unlist(LinkedRecords.SID)),
#         by=.(SourceName, SourceID, Name, DILI)]
#     return(DT)
# }

# # #' @importFrom data.table data.table as.data.table merge.data.table last rbindlist
# .parseNSCannotations <- function(DT) {
#     DT[, NSC := unlist(lapply(Data, `[[`, i=4))]
#     annotationDT <- DT[,
#         .(CID=unlist(LinkedRecords.CID), SID=unlist(LinkedRecords.SID)),
#         by=.(SourceName, SourceID, NSC)]
#     return(annotationDT)
# }

# #' @importFrom data.table data.table as.data.table merge.data.table last rbindlist
# .parseCTDannotations <- function(DT) {
#     annotationDT <- DT[, .(CID=unlist(LinkedRecords)),
#         by=.(SourceName, SourceID, URL)]
#     annotationDT[, CTD := gsub('::.*$', '', SourceID)]
#     return(annotationDT)
# }

# #' @importFrom data.table data.table as.data.table merge.data.table last rbindlist setnames
# .parseCASannotations <- function(list) {
#     # Make sure CIDs all go in the same column
#     CAS_list <- lapply(list, setnames, old='LinkedRecords.CID', new='LinkedRecords',
#         skip_absent=TRUE)
#     DT <- rbindlist(CAS_list, fill=TRUE, use.names=TRUE)
#     DT[, CAS := lapply(Data, function(x) unlist(x[[2]]))]
#     CAS_DT <- DT[, .(CAS=unlist(CAS)), by=.(SourceName, SourceID, Name)]
#     ID_DT <- DT[, 
#         .(CID=unlist(lapply(LinkedRecords, function(x) if(is.null(x)) NA_integer_ else x))),
#         by=.(SourceName, SourceID, Name, URL)]
#     annotationDT <- merge.data.table(
#         CAS_DT, ID_DT,
#         by=c('SourceName', 'SourceID', 'Name'), 
#         all.x=TRUE,
#         allow.cartesian=TRUE)
#     annotationDT <- unique(annotationDT)
#     return(annotationDT)
# }

# #' @importFrom data.table data.table as.data.table merge.data.table last rbindlist
# .parseSynonymsAndIdentifiers <- function(DT) {
#     dataList <- lapply(DT$Data, as.data.table)
#     names(dataList) <- DT$SourceID
#     dataDT <- rbindlist(dataList, fill=TRUE, use.names=TRUE,
#         idcol='SourceID')
#     DT[, Data := NULL]
#     dataDT[,
#         Synonyms := paste0(unlist(Value.StringWithMarkup[[1]]), collapse='|'),
#         by=SourceID]
#     dataDT[, Synonyms := paste(Synonyms, '|', Name), by=SourceID]
#     dataDT[, Value.StringWithMarkup := NULL]
#     annotationDT <- merge.data.table(dataDT, DT, by='SourceID', allow.cartesian=TRUE)
#     setnames(annotationDT,
#         old=c('TOCHeading.type', 'TOCHeading..TOCHeading', 'LinkedRecords'),
#         new=c('Type', 'Heading', 'ID')
#     )
#     annotationDT <- unique(annotationDT)
#     return(annotationDT)
# }

# #' @importFrom data.table data.table as.data.table merge.data.table last rbindlist
# .parseNamesAndSynonyms <- function(DT) {
#     DT[, Synonyms := lapply(Data, function(x) x[2, ]$Value[[1]][[1]])]
#     # Remove the weird annotation from the end of the synonym
#     DT[, Synonyms := lapply(Synonyms, FUN=gsub, pattern=' - .*$', replacement='')]
#     DT[, Synonyms := unlist(lapply(Synonyms, FUN=paste0, collapse='|'))]
#     # fix NULL list itemss
#     DT[, CID := lapply(LinkedRecords.CID, function(x) if(is.null(x)) NA_integer_ else x)]
#     DT[, SID := lapply(LinkedRecords.SID, function(x) if(is.null(x)) NA_integer_ else x)]
#     annotationDT <- DT[, .(CID=unlist(CID), SID=unlist(SID)),
#         by=.(SourceName, SourceID, Name, URL, Synonyms)]
#     annotationDT <- unique(annotationDT)
#     return(annotationDT)
# }
