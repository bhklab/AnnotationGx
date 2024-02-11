#' ignore_unused_imports function
#'
#' This function is used to ignore unused imports.
#' It calls the crayon::green function and the httr2::request function.
#'
#' @return None
ignore_unused_imports <- function() {
    crayon::green
    httr2::request()
}