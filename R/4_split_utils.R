
# The following functions are taken from the AcidBase package by acidgenomics using their
# license. Adding the package as a dependency is the better approach but fails on the 
# CI/CD pipeline as the package is not available on CRAN.
# TODO:: Add the package as a dependency and remove the following functions.
# TODO:: reach out to the author to discuss the license and the possibility of 
#        adding the package as a dependency.

#' Split a character vector into a matrix based on a delimiter
#'
#' This function splits a character vector into a matrix based on a specified delimiter.
#' It can handle both finite and infinite splits.
#'
#' @param x A character vector to be split
#' @param split A character string specifying the delimiter
#' @param fixed A logical value indicating whether the delimiter should be treated as a fixed string
#' @param n An integer specifying the maximum number of splits to be performed
#'
#' @return A matrix where each row represents a split element
#'
#' @examples
#' strSplit("Hello,World", ",")
#' # Output:
#' #      [,1]    [,2]   
#' # [1,] "Hello" "World"
#'
#' @export
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


#' Split a string into multiple substrings based on a delimiter
#'
#' This function splits a given string into multiple substrings based on a specified delimiter.
#' The number of resulting substrings can be controlled using the 'n' parameter.
#'
#' @param x The input string to be split.
#' @param split The delimiter used to split the string.
#' @param n The maximum number of substrings to be generated.
#' @param fixed A logical value indicating whether the 'split' parameter should be treated as a fixed string or a regular expression.
#'
#' @return A character vector containing the resulting substrings.
#'
#' @examples
#' str <- "Hello,World,How,Are,You"
#' .strSplitFinite(str, ",", 3, fixed = TRUE)
#'
#' @noRd
#' @keywords internal
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


#' Split a character vector into substrings based on a delimiter
#'
#' This function splits a character vector into substrings based on a specified delimiter.
#' It uses the `strsplit` function from the base R package.
#'
#' @param x A character vector to be split.
#' @param split A character string specifying the delimiter to use for splitting.
#' @param fixed A logical value indicating whether the delimiter should be treated as a fixed string.
#'              If `TRUE`, the delimiter is treated as a fixed string; if `FALSE`, it is treated as a regular expression.
#'
#' @return A list of character vectors, where each element of the list corresponds to the substrings obtained from splitting the input vector.
#'
#' @examples
#' x <- c("apple,banana,orange", "cat,dog,rabbit")
#' strSplitInfinite(x, ",", fixed = TRUE)
#'
#' @noRd
#' @keywords internal
.strSplitInfinite <- function(x, split, fixed) {
    checkmate::assertCharacter(x)
    checkmate::assertString(split)
    checkmate::assertFlag(fixed)
    strsplit(x = x, split = split, fixed = fixed)
}



#' Split a column into a character list
#'
#' @note Updated 2023-09-22.
#' @noRd
.splitCol <- function(object, colName, split = "; ") {
  checkmate::assert_class(object, "data.table")
  object[[colName]] <- strsplit(object[[colName]], split = split, fixed = TRUE)
  object
}

#' Split a nested column by key
#'
#' Don't format key names into camel case -- too CPU intensive.
#'
#' @note Updated 2023-09-22.
#' @noRd
.splitNestedCol <- function(object, colName, split) {
    # assert(
    #     is(object, "DFrame"),
    #     is(object[[colName]], "CharacterList"),
    #     isString(split)
    # )
    lst <- lapply(
        X = object[[colName]],
        split = split,
        FUN = function(x, split) {
            if (identical(x, character())) {
                return(list())
            }
            x <- strSplit(x = x, split = split, n = 2L)
            ## Formatting into camel case takes too long.
            ## > x[, 1L] <- camelCase(x[, 1L])
            x <- split(x = x[, 2L], f = x[, 1L])
            x
        }
    ) |> unlist(recursive = F)
    object[[colName]] <- lst
    object
}
