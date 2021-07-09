#' @import R6
#' @importFrom R6P Singleton
NULL

# Singleton ProxyManager Class to Manage a List of Proxy URLs
#
# @description
# # Fields
# name `character(1)` A string name for the ProxyManager. Defaults to
# 'ProxyManger'
#
# @details
# ## Constructor
#
#' @md
#' @importFrom R6P Singleton
#' @importFrom httr GET
#' @importFrom data.table data.table fread copy
#' @importFrom crayon magenta cyan bold
#' @importFrom listenv listenv
#' @export
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
            proxy_source_url='https://api.proxyscrape.com/v2/?request=getproxies&protocol=http&timeoust=10000&country=all&ssl=yes&anonymity=all&simplified=true',
            name='ProxyManager', ..., 
            proxy_parser=parse_text_response_to_datatable)
        {
            # Initialze the object fields
            private$name <- name
            private$proxy_source_url <- proxy_source_url
            private$failed_proxies <- data.table::data.table(failed=integer())

            # Get the data from the source URL
            tryCatch({
                response <- httr::GET(proxy_source_url, ...)
            },
            error=function(e) stop('The GET request failed with: ', e))

            # Parse the text into a data.frame
            tryCatch({
                private$proxy_data <- proxy_parser(response)
            },
            error=function(e) stop('The proxy_parser function failed with: ', e))
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


