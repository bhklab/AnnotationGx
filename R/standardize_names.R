

#' Standardize Names
#'
#' This function takes a character vector and standardizes the names by converting them to lowercase,
#' removing any trailing information after a comma, removing any information within square brackets or parentheses,
#' removing any non-alphanumeric characters, replacing empty names with "invalid", and converting the names to uppercase.
#'
#' @param object A character vector containing the names to be standardized.
#' @return A character vector with the standardized names.
#' @examples
#' standardize_names(c("John Doe", "Jane Smith (Manager)", "Alice, PhD"))
#' # Output: [1] "JOHNDOE" "JANESMITH" "ALICE"
#' @export
standardize_names<- function(object) {
    checkmate::assert_character(object, all.missing=F)
    object <- tolower(object)
    object <- gsub(
        pattern = ",\\s.+$",
        replacement = "",
        x = object
    )
    object <- sub(
        pattern = "\\s[\\[\\(].+$",
        replacement = "",
        x = object
    )
    object <- gsub(
        pattern = "[^[:alnum:]]+",
        replacement = "",
        x = object
    )
    if (any(object == "")) {
        object[object == ""] <- NA
    }
    object <- toupper(object)
    object
}