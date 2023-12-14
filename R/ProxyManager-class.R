#' @import R6
#' @importFrom R6P Singleton
NULL

# Singleton ProxyManager Class to Manage a List of Proxy URLs
#
# @description
# # Fields
# name `character(1)` A string name for the ProxyManager. Defaults to
#   'ProxyManger'
# proxy_source_url `character(1)` URL for a source of free proxies.
# proxy-data `data.table` Table of free proxies, initialized after calling
#   the `ProxyManager$connect()` method.
# failed_proxies `data.table` Collection of all the proxies that have
#   had a failed request and are therefore removed from the `proxy_data` field.
#
#' @importFrom R6P Singleton
#' @importFrom httr GET
#' @importFrom data.table data.table fread copy
#' @importFrom crayon magenta cyan bold
#' @importFrom listenv listenv
#'
#' @noRd
#' @keywords internal
ProxyManager <- R6::R6Class('ProxyManager',
    inherit=R6P::Singleton,
    private=list(
        name='character',
        proxy_source_url='character',
        proxy_data='data.table',
        failed_proxies='data.table'
    ),
    public=list(
        initialize=function(
            proxy_source_url='https://api.proxyscrape.com/v2/?request=displayproxies&protocol=http&timeout=10000&country=all&ssl=all&anonymity=all',
            name='ProxyManager', ...,
            proxy_parser=parse_text_response_to_datatable)
        {
            # Initialze the object fields
            private$name <- name
            private$proxy_source_url <- proxy_source_url
            private$proxy_data <- NULL
            private$failed_proxies <- data.table::data.table(failed=integer())
        },
        connect=function(...) {
            # Get the data from the source URL
            response <- httr::GET(private$proxy_source_url, ...)
            # Parse the text into a data.frame
            private$proxy_data <- proxy_parser(response)
        },
        get_proxies=function() {
            data.table::copy(private$proxy_data)
        },
        update_failed_proxies=function(new_data) {
            stopifnot(is.numeric(new_data))
            private$failed_proxies <- private$failed_proxies[,
                failed = c(failed, as.integer(new_data))]
        },
        get_failed_proxies=function() {
            unique(unlist(private$failed_proxies))
        }
    )
)

#' Parse text response to create a data table of proxies.
#'
#' This function takes a response object and parses the text content to create a data table
#' of proxies. It imports the necessary functions from the `data.table` and `httr` packages.
#' The response object is expected to contain a text response that is formatted with IP addresses
#' and ports separated by a colon. The function splits the text response into separate columns
#' for IP and port, and adds a 'http://' prefix to the IP addresses. The resulting data table
#' has two columns: 'ip' and 'port'.
#'
#' @param response The response object containing the text response.
#' @return A data table with columns 'ip' and 'port' representing the parsed proxies.
#' @importFrom data.table data.table tstrsplit as.data.table
#' @importFrom httr content
#' @export
parse_text_response_to_datatable <- function(response) {
    proxyDT <- data.table::fread(httr::content(response), header=FALSE)
    proxyDT <- data.table::as.data.table(
        data.table::tstrsplit(proxyDT$V1, ':'))
    colnames(proxyDT) <- c('ip', 'port')
    proxyDT[['ip']] <- paste0('http://', proxyDT$ip)
    return(proxyDT)
}
