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
getPubChemStatus <- function(sleep = FALSE, throttleMessage = FALSE){
    
    response <- tryCatch(
    {
        httr::GET("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/Aspirin/cids/JSON")
    },  
        error = function(e) {
            message("Error: ", e$message)
            return(NULL)
    })

    if (!is.null(response)) {
        status_code <- httr::status_code(response)
        # message("Status code: ", status_code)
        if (status_code == 503) {
            message("The server is currently unable to
             handle the request due to overload")
        } else if(http_error(response)){
            message("HTTP error: ", status_code)
        } else {
            return(.checkThrottlingStatus2(response, sleep, throttleMessage))
        }
    } else {
        return(NULL)
    }
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

#' @keywords internal
.checkThrottlingStatus2 <- function(response, sleep=TRUE, throttleMessage = TRUE){
    message <- headers(response)$`x-throttling-control`
    parsed_info <- .parse_throttling_message(message)
    if(throttleMessage){
        message("Throttling status: ", parsed_info)
    }
    # names are: request_count, request_time and service
    # each has status and percent
    # main throttlers for user are request_count and request_time
    # main statuses are:
        # Green - less than 50% of the permitted request limit has been used
        # Yellow - between 50% and 75% of the request limit has been used
        # Red - more than 75% of the request limit has been reached
        # Black - the limit has been exceeded and requests are being blocked

    # Check if the request count or request time is 
    
    if(parsed_info$service$status == "Black"){
        message("WARNING: The request limit has been exceeded and requests are being blocked.")
        Sys.sleep(60)
    }else if(parsed_info$service$status %in% c("Red", "Yellow")){
        message("WARNING: The request limit has been reached or is close to being reached.")
        Sys.sleep(15)
    }   
    
    max_request_percent <- max(parsed_info$request_count$percent, parsed_info$request_time$percent)
    
    # if any of the statuses are red, sleep for 60 seconds
    if (parsed_info$request_count$status == "Red" | parsed_info$request_time$status == "Red") {
        message("WARNING: Request count or request time is red. Sleeping for 60 seconds.")
        sleep_time <- 60
    } else {
        # If % > 50, sleep for 30 seconds, elseif % > 30, sleep for 20 seconds, else sleep for 15 seconds
        sleep_time <- ifelse(max_request_percent > 50, 30, ifelse(max_request_percent > 30, 15, 1))
    }
    if(sleep) Sys.sleep(sleep_time)
    return(sleep_time >1)
}
getPubChemStatus(F, F)

dt <- fread("sandbox/GDSC2_8.4_treatmentMetadata_annotated.tsv")


BPPARAM <- BiocParallel::MulticoreParam(workers = 5, progressbar = TRUE, stop.on.error = FALSE)
subdt <- unique(dt[,GDSC.treatmentid])[100:200]
compound_nameToCIDS <- AnnotationGx::getPubChemCompound(
    subdt,
    from='name',
    to='cids',
    batch = FALSE,
    verbose = FALSE,
    query_only = TRUE,
    BPPARAM = BiocParallel::MulticoreParam(workers = 10, progressbar = TRUE, stop.on.error = FALSE)
)

BiocParallel::bplapply(compound_nameToCIDS, function(x){
    response <- httr::GET(x)
    if(.checkThrottlingStatus2(response, TRUE, FALSE)){
        print(httr::headers(response)$`x-throttling-control`)
    }
    parseJSON(response)
}, BPPARAM = BPPARAM)



# STATUS REQUEST PARSER
# HTTP Status       Error Code	            General Error Category
# 200	            (none)	                Success
# 202	            (none)	                Accepted (asynchronous operation pending)
# 400	            PUGREST.BadRequest	    Request is improperly formed (syntax error in the URL, POST body, etc.)
# 404	            PUGREST.NotFound	    The input record was not found (e.g. invalid CID)
# 405	            PUGREST.NotAllowed	    Request not allowed (such as invalid MIME type in the HTTP Accept header)
# 504	            PUGREST.Timeout	The     request timed out, from server overload or too broad a request
# 503	            PUGREST.ServerBusy	    Too many requests or server is busy, retry later
# 501	            PUGREST.Unimplemented	The requested operation has not (yet) been implemented by the server
# 500	            PUGREST.ServerError	    Some problem on the server side (such as a database server down, etc.)
# 500	            PUGREST.Unknown	        An unknown error occurred

.parse_pubchem_status_code <- function(status_code){
    err <- switch(status_code,
        "200" = "Success",
        "202" = "Accepted (asynchronous operation pending)",
        "400" = "Request is improperly formed (syntax error in the URL, POST body, etc.)",
        "404" = "The input record was not found (e.g. invalid CID)",
        "405" = "Request not allowed (such as invalid MIME type in the HTTP Accept header)",
        "504" = "The request timed out, from server overload or too broad a request",
        "503" = "Too many requests or server is busy, retry later",
        "501" = "The requested operation has not (yet) been implemented by the server",
        "500" = "Some problem on the server side (such as a database server down, etc.)",
        "500" = "An unknown error occurred"
    )
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
