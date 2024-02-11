#' Default Log formatter
#'
#' @title Default Log formatter
#' @description log_fmt function to format log messages
#' @param level `character` The log level
#' @param ... `character` The messages to log
#' @keywords internal
#' @noRd
.log_fmt <- function(level, ...) {
  paste0(format(Sys.time(), "[%H:%M:%S]"), " [", level, "] ", ..., collapse = "\n")
}


#' Custom message function for verbose output
#'
#' This function is used to print messages when the verbose option is enabled.
#' It checks if the package-specific verbose option is set or if the global verbose option is set.
#' If either of these options is TRUE, the message is printed in blue and bold format.
#'
#' @param ... `character` The messages to print
#'
#' @examples
#' \dontrun{
#' options("myPackage.verbose" = TRUE)
#' }
#'
#' @keywords internal
#' @noRd
.info <- function(...) {
    msg <- .log_fmt("INFO", ...)
    # optionName <- paste0(packageName(), ".verbose")
    optionIsTRUE <- options::opt("log_level") == "INFO"
    # verboseIsTRUE <- getOption("verbose")
    # if (optionIsTRUE || verboseIsTRUE)
    message(crayon::green(msg))
}

#' @keywords internal
#' @noRd
.debug <- function(...) {
    msg <- .log_fmt("DEBUG", ...)
    # optionName <- paste0(packageName(), ".debug")
    # # to set the debug option, use options("myPackage.debug" = TRUE)
    optionIsTRUE <- options::opt("log_level") == "DEBUG"
    # optionIsTRUE <- !is.null(getOption(optionName)) && getOption(optionName)
    verboseIsTRUE <- options::opt("verbose")
    if (optionIsTRUE || verboseIsTRUE)
        message(crayon::blue(msg))
}

#' @keywords internal
#' @noRd
.warn <- function(...) {
    msg <- .log_fmt("WARNING", ...)

    optionIsTRUE <- options::opt("log_level") != "ERROR"
    message(crayon::yellow(msg))
}

#' @keywords internal
#' @noRd
.err <- function(...) {
    msg <- .log_fmt("ERROR", ...)
    optionIsTRUE <- options::opt("log_level") != NULL
    stop(crayon::red(msg), call. = FALSE)
}



# # test
# .debug("This is a debug message")
# .info("This is an info message")
# .warn("This is a warning message")
# .error("This is an error message")


#' Generate a function context string
#'
#' This function takes the name of a function and returns a string that
#' represents the function context.
#' The string is formatted as [packageName functionName].
#'
#' @keywords internal
#' @noRd
.funContext <- function(funName) paste0("[", utils::packageName(), "::", funName, "]")
