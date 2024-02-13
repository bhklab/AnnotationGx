#' ignore_unused_imports function
#'
#' This function is used to ignore unused imports.
#' It calls the crayon::green function and the httr2::request function.
#'
#' @return None
#' @keywords internal
ignore_unused_imports <- function() {
    #TODO:: FIGURE OUT HOW TO NOT NEED THIS
    crayon::green
    httr2::request()
    ignore <- testthat::expect_equal(1,1)
}