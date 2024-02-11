options::define_option(
  option = "log_level",
  default = "WARNING",
  desc = paste0(
    "The log level to use. Possible values are 'ERROR', 'WARNING', 'DEBUG', 'INFO' ",
    "Default is 'WARNING'."
  ),
  option_name = "annotationgx_log_level", # define custom option names
  envvar_name = "ANNOTATIONGX_LOGLEVEL" # and custom environment variable names
)

#' @eval options::as_roxygen_docs()
NULL
options::define_option(
  option = "verbose",
  default = FALSE,
  desc = paste0(
    "The verbosity level to use. Default is FALSE."
  ),
  option_name = "annotationgx_verbose", # define custom option names
  envvar_name = "ANNOTATIONGX_VERBOSE" # and custom environment variable names
)