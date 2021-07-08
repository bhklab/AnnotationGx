#' @importFrom data.table data.table fwrite :=
.onLoad <- function(libname, packagename) {
    # Get a list of proxies to use
    request <- httr::GET('https://api.proxyscrape.com/v2/?request=getproxies&protocol=http&timeoust=10000&country=all&ssl=yes&anonymity=all&simplified=true')
    proxyDT <- data.table::fread(httr::content(request), header=FALSE)
    proxyDT <- data.table::as.data.table(
        data.table::tstrsplit(proxyDT$V1, ':'))
    colnames(proxyDT) <- c('ip', 'port')
    proxyDT[['ip']] <- paste0('http://', proxyDT$ip)
    data.table::fwrite(proxyDT, 
        file=file.path(tempdir(), 'proxy.csv'))
}