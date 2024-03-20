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
.bplapply <- function(X, FUN, ..., BPPARAM = BiocParallel::SerialParam()) {
  BiocParallel::bplapply(X, FUN, ..., BPPARAM = BPPARAM)
}


#' Parses the query response into a data table
#'
#' This function takes a query response and converts it into a data table using the `as.data.table` function from the `data.table` package.
#'
#' @param resp The query response to be parsed
#' @return A data table containing the parsed query response
#'
#' @noRd
#' @keywords internal
.parseQueryToDT <- function(resp) {
  data.table::as.data.table(resp[[1]][[1]])
}


strSplit <- function(x, split, fixed = TRUE, n = Inf) {

    if (is.finite(n)) {
        x <- .strSplitFinite(x = x, split = split, n = n, fixed = fixed)
    } else {
        x <- .strSplitInfinite(x = x, split = split, fixed = fixed)
    }
    n2 <- lengths(x)
    assert(
        length(unique(n2)) == 1L,
        msg = sprintf(
            "Split mismatch detected: %s.",
            toString(which(n2 != n2[[1L]]))
        )
    )
    n2 <- n2[[1L]]
    x <- unlist(x = x, recursive = FALSE, use.names = FALSE)
    x <- matrix(data = x, ncol = n2, byrow = TRUE)
    x
}




#' Split a string into a finite number of capture groups
#'
.strSplitFinite <- function(x, split, n, fixed) {

    checkmate::assertString(split)
    checkmate::assertFlag(fixed)
    checkmate::assert_integerish(n, lower = 2L, upper = Inf)
    checkmate::assert_character(x)

    m <- gregexpr(pattern = split, text = x, fixed = fixed)
    ln <- lengths(m)
    assert(
        all((ln + 1L) >= n),
        msg = sprintf(
            "Not enough to split: %s.",
            toString(which((ln + 1L) < n))
        )
    )
    Map(
        x = x,
        m = m,
        n = n,
        f = function(x, m, n) {
            ml <- attr(m, "match.length")
            nl <- seq_len(n)
            m <- m[nl]
            ml <- ml[nl]
            out <- substr(x = x, start = 1L, stop = m[[1L]] - 1L)
            i <- 1L
            while (i < (length(m) - 1L)) {
                out <- append(
                    x = out,
                    values = substr(
                        x = x,
                        start = m[[i]] + ml[[i]],
                        stop = m[[i + 1L]] - 1L
                    )
                )
                i <- i + 1L
            }
            out <- append(
                x = out,
                values = substr(
                    x = x,
                    start = m[[n - 1L]] + ml[[n - 1L]],
                    stop = nchar(x)
                )
            )
            out
        },
        USE.NAMES = FALSE
    )
}




#' Split a string into an finite number of capture groups
.strSplitInfinite <- function(x, split, fixed) {
    checkmate::assertCharacter(X)
    checkmate::assertString(split)
    checkmate::assertFlag(fixed)
    strsplit(x = x, split = split, fixed = fixed)
}