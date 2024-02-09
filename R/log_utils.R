#' Default Log formatter
#'
#' @title Default Log formatter
#' @description log_fmt function to format log messages
#' @param level `character` The log level
#' @param ... `character` The messages to log
#' @keywords internal
#' @noRd
.log_fmt <- function(level, ...) {
  paste0(format(Sys.time(), "[%H-%M-%S]"), " [", level, "] ", ..., collapse = "\n")
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
    # optionName <- paste0(packageName(), ".verbose")
    # optionIsTRUE <- !is.null(getOption(optionName)) && getOption(optionName)
    # verboseIsTRUE <- getOption("verbose")
    # if (optionIsTRUE || verboseIsTRUE)
    #     message(crayon::green(.log_fmt("INFO", ...)))
    message(crayon::green(.log_fmt("INFO", ...)))
}

#' @keywords internal
#' @noRd
.debug <- function(...) {
    msg <- .log_fmt("DEBUG", ...)
    message(crayon::blue(msg))
}

#' @keywords internal
#' @noRd
.warn <- function(...) {
    msg <- .log_fmt("WARN", ...)
    warning(crayon::yellow(msg), call. = FALSE)
}

#' @keywords internal
#' @noRd
.error <- function(...) {
    msg <- .log_fmt("ERROR", ...)
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

#' Return the name of the function and the name of the package that function
#'   is in when called within an R function.
#'
#' For providing context in user messages, warnings and errors
#'
#' @param n `integer` How far up the call stack to look for context. Defaults to
#'   2 since it is assumed this function will be used inside of `message`,
#'   `warning` or `stop`.
#'
#' @return `list`:
#' - fun: `character` The name of the function where `.getExectutionContext()`
#' was called
#' - pkg: `character` The name of the package `fun` is from, if applicable.
#'
#' @md
#' @keywords internal
#' @noRd
# .getExecutionContext <- function(n=2) {

#     # name of function which called this function
#     callStack <- rlang::trace_back()$calls
#     context <- deparse(callStack[[length(callStack) - n]])

#     # remove function arguments
#     context <- gsub('\\(.*\\)', '', context)

#     return(paste0('\n[', context, '] ', collapse='::'))
# }



