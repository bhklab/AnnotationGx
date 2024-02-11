.buildURL <- function(...) {
    paste0(stats::na.omit(unlist(list(...))), collapse='/') |> utils::URLencode()
}


.build_pubchem_request <- function(url){
    httr2::request(url) |>
        httr2::req_retry(max_tries = 3) |>
        httr2::req_throttle(rate = 1000/60) |>
        httr2::req_error(is_error = \(resp) FALSE)
}
d

.parse_resp_json <- function(resp){
    httr2::resp_body_json(resp, simplifyVector = TRUE)
}

.parseQueryToDT <- function(resp){
    data.table::as.data.table(resp[[1]][[1]])
}


# # getPubchemStatus <- function(sleep = FALSE, throttleMessage = FALSE){
    
# #     response <- tryCatch(
# #     {
# #         httr::GET("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/Aspirin/cids/JSON")
# #     },
# #         error = function(e) {
# #             message("Error: ", e$message)
# #             return(NULL)
# #     })

# #     if (!is.null(response)) {
# #         status_code <- httr::status_code(response)
# #         # message("Status code: ", status_code)
# #         if (status_code == 503) {
# #             message("The server is currently unable to
# #              handle the request due to overload")
# #         } else if(http_error(response)){
# #             message("HTTP error: ", status_code)
# #         } else {
# #             return(.checkThrottlingStatus2(response, sleep, throttleMessage))
# #         }
# #     } else {
# #         return(NULL)
# #     }
# # }

# #' @keywords internal
# .parse_throttling_message <- function(message) {
#   # Split the message into components
#   components <- strsplit(message, ", ")[[1]]
  
#   # Initialize an empty list to store the parsed information
#   parsed_info <- list()
  
#   # Loop through each component and extract the relevant information
#   for (comp in components) {
#     # Split each component into key-value pairs
#     kv <- strsplit(comp, ": ")[[1]]
#     key <- tolower(gsub(" status", "", kv[1]))
#     key <- gsub(" ", "_", key)
#     value <- kv[2]
    
#     # Extract status and percent
#     status <- sub("\\s*\\(.*\\)", "", value)
#     percent <- as.integer(sub(".*\\((\\d+)%\\).*", "\\1", value))
    
#     # Store the extracted information in the parsed_info list
#     parsed_info[[key]] <- list(status = status, percent = percent)
#   }
  
#   return(parsed_info)
# }

# #' @keywords internal
# .checkThrottlingStatus2 <- function(response, sleep=TRUE, throttleMessage = TRUE){
#     message <- httr2::resp_headers(response)$`x-throttling-control`
#     parsed_info <- .parse_throttling_message(message)
#     if(throttleMessage){
#         message("Throttling status: ", parsed_info)
#     }
#     # names are: request_count, request_time and service
#     # each has status and percent
#     # main throttlers for user are request_count and request_time
#     # main statuses are:
#         # Green - less than 50% of the permitted request limit has been used
#         # Yellow - between 50% and 75% of the request limit has been used
#         # Red - more than 75% of the request limit has been reached
#         # Black - the limit has been exceeded and requests are being blocked

#     # Check if the request count or request time is
    
#     if(parsed_info$service$status == "Black"){
#         message("WARNING: The request limit has been exceeded and requests are being blocked.")
#         Sys.sleep(60)
#     }else if(parsed_info$service$status %in% c("Red", "Yellow")){
#         message("WARNING: The request limit has been reached or is close to being reached.")
#         Sys.sleep(15)
#     }
    
#     max_request_percent <- max(parsed_info$request_count$percent, parsed_info$request_time$percent)
    
#     # if any of the statuses are red, sleep for 60 seconds
#     if (parsed_info$request_count$status == "Red" | parsed_info$request_time$status == "Red") {
#         message("WARNING: Request count or request time is red. Sleeping for 60 seconds.")
#         sleep_time <- 60
#     } else {
#         # If % > 50, sleep for 30 seconds, elseif % > 30, sleep for 20 seconds, else sleep for 15 seconds
#         sleep_time <- ifelse(max_request_percent > 50, 30, ifelse(max_request_percent > 30, 15, 1))
#     }
#     if(sleep) Sys.sleep(sleep_time)
#     return(sleep_time >1)
# }