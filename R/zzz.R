.onLoad <- function(libname, packagename) {
    # initialize the proxy manager reference class in the pkg namespace
    delayedAssign(
        'proxyManager',
        AnnotationGx:::ProxyManager$new(),
        assign.env=asNamespace(packagename)
    )
}