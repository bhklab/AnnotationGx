
#' simple wrapper for the data.table::as.data.table() function
#' @param x object to convert to a data.table
#' @param ... additional arguments to pass to data.table::as.data.table()
#' @return a data.table
#' @keywords internal
#' @noRd
.asDT <- function(x, ...) data.table::as.data.table(x, ...)



#' Custom wrapper function for parallelizing lapply using BiocParallel.
#'
#' This function provides a convenient way to parallelize the lapply function
#' using the BiocParallel package. It takes a list or vector \code{X} and applies
#' the function \code{FUN} to each element in parallel. The parallelization is
#' controlled by the \code{BPPARAM} argument, which defaults to the SerialParam
#' object from BiocParallel.
#'
#' @param X A list or vector to apply the function to.
#' @param FUN The function to apply to each element of \code{X}.
#' @param ... Additional arguments to pass to \code{FUN}.
#' @param BPPARAM A BiocParallel parameter object controlling the parallelization.
#' @inheritParams BiocParallel::bplapply
#' @return A list containing the results of applying \code{FUN} to each element of \code{X}.
#'
#' @import BiocParallel
#'
#' @examples
#' # Apply a function to a list in parallel
#' x <- list(1, 2, 3, 4, 5)
#' .bplapply(x, function(x) x^2)
#'
#' @keywords internal
#' @noRd
.bplapply <- function(X, FUN, ..., BPPARAM = BiocParallel::SerialParam()){
    BiocParallel::bplapply(X, FUN, ..., BPPARAM = BPPARAM)
}
