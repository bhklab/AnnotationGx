#' Default Log formatter
#'
#' @title Default Log formatter
#' @description log_fmt function to format log messages
#' @param level `character` The log level
#' @param ... `character` The messages to log
#' @keywords internal
#' @noRd
.log_fmt <- function(level, ...) {
  time_str <- format(Sys.time(), "[%H:%M:%S]")
  level_str <- paste0("[", level, "]")
  msg <- paste(..., sep = " ")
  paste0(time_str, level_str, msg, sep = " ", collapse = "\n")
}


#' Info message function
#' 
#' This function is used to print messages when the verbose option is enabled.
#' 
#' @param ... `character` The messages to print
#' 
#' @keywords internal
#' @noRd
#' @export
#' @examples
#' \dontrun{
#' .info("This is an info message")
#' }
.info <- function(...) {
  msg <- .log_fmt("INFO", ...)
  optionIsTRUE <- options("log_level") == "INFO" || (options("log_level") %in% c("WARN", "DEBUG", "ERROR"))
  if(optionIsTRUE) {
    message(crayon::green(msg))
  }
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
.debug <- function(...) {
  msg <- .log_fmt("DEBUG", ...)
  optionIsTRUE <- options("log_level") == "DEBUG"
  if (optionIsTRUE) {
    message(crayon::blue(msg))
  }
}

#' @keywords internal
#' @noRd
.warn <- function(...) {
  msg <- .log_fmt("WARNING", ...)
  optionIsTRUE <- options("log_level") != "ERROR"
  message(crayon::yellow(msg))
}

#' @keywords internal
#' @noRd
.err <- function(...) {
  msg <- .log_fmt("ERROR", ...)
  optionIsTRUE <- options("log_level") != NULL
  stop("\r", crayon::red(msg), call. = FALSE)
}

#' Generate a function context string
#'
#' This function takes the name of a function and returns a string that
#' represents the function context.
#' The string is formatted as [packageName functionName].
#'
#' @keywords internal
#' @noRd
.funContext <- function(funName) paste0("[", utils::packageName(), "::", funName, "]")

# # test
# .debug("This is a debug message")
# .info("This is an info message")
# .warn("This is a warning message")
# .error("This is an error message")
