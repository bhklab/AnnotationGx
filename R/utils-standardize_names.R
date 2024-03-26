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
standardize_names <- function(object) {
  checkmate::assert_character(object, all.missing = F)
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


#' Clean character strings by removing special characters and formatting.
#'
#' This function takes a character string as input and performs several cleaning operations
#' to remove special characters, formatting, and unwanted substrings. The cleaned string
#' is then returned as the output.
#'
#' @param name A character string to be cleaned.
#' @param space_action A character vector specifying the actions to be taken for space characters.
#'                     One of c("", "-", " ").
#' @return The cleaned character string.
#'
#' @examples
#' cleanCharacterStrings("Cisplatin: 1 mg/mL (1.5 mM); 5 mM in DMSO")
#'
#' @export
cleanCharacterStrings <- function(name, space_action = "") {

  # make sure name is a string
  name <- as.character(name)

  # replace space characters based on space_action
  if (space_action == "-") {
    name <- gsub(" ", "-", name)
  } else if (space_action == " ") {
    name <- gsub(" ", " ", name)
  }else{
    name <- gsub(" ", "", name)
  }

  # remove the ~ character
  name <- gsub("~", "", name)

  # if there is a colon like in "Cisplatin: 1 mg/mL (1.5 mM); 5 mM in DMSO"
  # remove everything after the colon
  name <- gsub(":.*", "", name)

  # remove ,  ;  -  +  *  $  %  #  ^  _  as well as any spaces
  name <- gsub("[\\,\\;\\+\\*\\$\\%\\#\\^\\_]", "", name, perl = TRUE)

  # remove hyphen 
  if (!space_action == "-")  name <- gsub("-", "", name)

  # remove substring of round brackets and contents
  name <- gsub("\\s*\\(.*\\)", "", name)

  # remove substring of square brackets and contents
  name <- gsub("\\s*\\[.*\\]", "", name)

  # remove substring of curly brackets and contents
  name <- gsub("\\s*\\{.*\\}", "", name)



  # convert entire string to uppercase
  name <- toupper(name)

  # dealing with unicode characters 
  name <- gsub("Unicode", "", iconv(name, "LATIN1", "ASCII", "Unicode"), perl=TRUE)

  name
}


