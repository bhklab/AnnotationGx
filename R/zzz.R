.onLoad <- function(libname, packagename) {
    # initialize the proxy manager reference class in the pkg namespace
    assign('proxyManager', value=AnnotationGx:::ProxyManager$new(),
        envir=asNamespace(packagename))

}