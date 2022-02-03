.onLoad <- function(libname, packagename) {
    # initialize the proxy manager reference class in the pkg namespace
    assign('proxyManager', value=AnnotationGx:::ProxyManager$new(),
        envir=asNamespace(packagename))

    # old implementation of proxies
    response <- httr::GET('http://api.proxyscrape.com/v2/?request=getproxies&protocol=http&timeoust=10000&country=all&ssl=no&anonymity=all&simplified=true')
    proxy <- AnnotationGx:::parse_text_response_to_datatable(response)
    data.table::fwrite(proxy, file=file.path(tempdir(), 'proxy.csv'))
}