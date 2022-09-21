.onLoad <- function(libname, packagename) {
    # initialize the proxy manager reference class in the pkg namespace
    delayedAssign(
        'proxyManager',
        value=tryCatch({
            AnnotationGx:::ProxyManager$new()
        },
        error=warning("ProxyManager setup failed! Proxies must be configured",
            "manually.")),
        assign.env=asNamespace(packagename)
    )
}