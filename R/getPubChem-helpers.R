#' Checks to see if the PubChem query is exceeding the throttling limit
#' @param response `httr::response`
#' @param throttleMessage `logical` whether to print the throttling message
#' @return `logical` whether the query is throttled
.checkThrottlingStatus <- function(result, throttleMessage = FALSE){
    message <- headers(result)$`x-throttling-control`

    if (throttleMessage == TRUE){
        message(message)
    }
    # Request Count status: Green (5%), Request Time status: Green (0%), Service status: Green (30%)


    matches <- regmatches(message, gregexpr("\\((.*?)%\\)", message))  # Extracts text within parentheses
    percentages <- as.numeric(gsub("\\(|%|\\)", "", unlist(matches[1:3])))
    percentage <- max(percentages[1:3])
    # percentage <- ifelse(percentages[3] > 75, percentages[3], max(percentages[1:2]))
    sleep_time <- ifelse(percentage > 50, 30, ifelse(percentage > 30, 20, 15))

    if (percentage > 15) {
        # message(paste0("Sleeping for ", sleep_time, " seconds"))
        Sys.sleep(sleep_time)
    } else {
        Sys.sleep(percentage)
    }

    return(percentage > 15)
}

#' simple checkThrottling Status by a default response 
#' @return `logical` whether the query is throttled
#' @export
getPubChemStatus <- function(sleep = FALSE){
    response <- httr::GET("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/Aspirin/cids/JSON")
    msg <- headers(response)$`x-throttling-control`
    matches <- regmatches(msg, gregexpr("\\((.*?)%\\)", msg))  # Extracts text within parentheses
    if(sleep) .checkThrottlingStatus(response)
    message(msg)
}


# -----------------------------
# getPubChemAnnotations Helpers

#' @importFrom data.table data.table as.data.table merge.data.table last rbindlist
.parseATCannotations <- function(DT) {
    dataL <- DT$Data
    names(dataL) <- DT$SourceID
    dataDT <- rbindlist(dataL, fill=TRUE, use.names=TRUE, idcol='SourceID')
    dataDT[, ATC_code := unlist(lapply(Value.StringWithMarkup,
        function(x) last(x)[[1]]))]
    annotationDT <- merge.data.table(
        dataDT[, .(SourceID, ATC_code)],
        DT[, .(SourceName, SourceID, LinkedRecords)],
        by='SourceID',
        allow.cartesian=TRUE
    )
    DT <- annotationDT[, .(CID=unlist(LinkedRecords)),
        by=.(SourceName, SourceID, ATC_code)]
    DT <- unique(DT)
    return(DT)
}

#' @importFrom data.table data.table as.data.table merge.data.table last rbindlist
.parseDILIannotations <- function(DT) {
    dataL <- DT$Data
    names(dataL) <- DT$SourceID
    dataL <- lapply(dataL, FUN=`[`, i=Name %like% 'DILI')
    dataDT <- rbindlist(dataL, fill=TRUE, use.names=TRUE, idcol='SourceID')
    dataDT[, DILI := unlist(Value.StringWithMarkup)]
    annotationDT <- merge.data.table(
        dataDT[, .(SourceID, DILI)],
        DT[, .(SourceID, SourceName, Name, LinkedRecords.CID,
            LinkedRecords.SID)],
        by='SourceID',
        allow.cartesian=TRUE)
    DT <- 
    annotationDT[, 
        .(CID=unlist(LinkedRecords.CID), SID=unlist(LinkedRecords.SID)),
        by=.(SourceName, SourceID, Name, DILI)]
    return(DT)
}

# #' @importFrom data.table data.table as.data.table merge.data.table last rbindlist
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
