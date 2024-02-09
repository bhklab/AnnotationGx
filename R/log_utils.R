#' These functions are to create some logging and debugging tools.
#' that will be used throughout the rest of the package.
#'
#'

# Would be beneficial to imnplement an environmental variable to set the logging level
    #  loglevel(2) == loglevel("INFO")
    #  loglevel("WARN") < loglevel("ERROR")
    #  loglevel(-1)
    #  try(loglevel("UNDEFINED"))
    #  is.loglevel("DEBUG")
    #  is.loglevel(loglevel("DEBUG"))
    #  as.numeric(loglevel("FATAL"))
    #  available.loglevels()

    #  ## Not run:

    #  library(optparse)
    #  library(log4r)

    #  optlist <- list(make_option(c('-v', '--verbosity-level'),
    #    type = "integer",
    #    dest = "verbosity",
    #    default = 1,
    #    help = "Verbosity threshold (5=DEBUG, 4=INFO 3=WARN, 2=ERROR, 1=FATAL)"))

    #  optparser <- OptionParser(option_list=optlist)
    #  opt <- parse_args(optparser)

    #  my.logger <- create.logger(logfile = "", level = verbosity(opt$verbosity))

    #  fatal(my.logger, "Fatal message")
    #  error(my.logger, "Error message")
    #  warn(my.logger, "Warning message")
    #  info(my.logger, "Informational message")
    #  debug(my.logger, "Debugging message")
    #  ## End(Not run)


#' Base Logging function to be inherited by other functions
#'
#' @param message The message to be logged
#'
#' Internal function
#'
#' @internal
parent_log <- function(level, message) {

    logger <- log4r::create.logger(logfile = "", level = level)


}

#' Debugging function to log messages at the debug level
#' Inherits from parent_log
#'
#' @param message The message to be logged
#'
#' @export

.debug <- function(message) {
  parent_log("debug", message)
}

.info <- function(message) {
  parent_log("info", message)
}

.warn <- function(message) {
  parent_log("warn", message)
}

.error <- function(message) {
  parent_log("error", message)
}

# test
.debug("This is a debug message")
.info("This is an info message")
.warn("This is a warning message")
.error("This is an error message")

