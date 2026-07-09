skip_if_unichem_unavailable <- local({
  state <- new.env(parent = emptyenv())
  state$checked <- FALSE
  state$available <- FALSE
  state$reason <- "UniChem availability has not been checked."

  function() {
    old_options <- options(
      annotationgx.request.timeout = 5,
      annotationgx.request.max_tries = 1
    )
    on.exit(options(old_options), add = TRUE)

    if (!state$checked) {
      state$checked <- TRUE
      state$available <- tryCatch(
        {
          req <- AnnotationGx::queryUnichemCompound(
            compound = "161671",
            type = "uci",
            request_only = TRUE
          )
          resp <- req |>
            AnnotationGx:::.perform_request()
          parsed <- AnnotationGx:::.parse_unichem_response(
            resp,
            request_label = "UniChem availability check",
            request = req
          )
          is.list(parsed)
        },
        error = function(e) {
          state$reason <- conditionMessage(e)
          FALSE
        }
      )
    }

    if (!state$available) {
      testthat::skip(paste("UniChem unavailable:", state$reason))
    }
  }
})

with_unichem_api <- function(expr) {
  old_options <- options(
    annotationgx.request.timeout = 5,
    annotationgx.request.max_tries = 1
  )
  on.exit(options(old_options), add = TRUE)

  tryCatch(
    force(expr),
    error = function(e) {
      msg <- conditionMessage(e)
      transient <- inherits(e, "httr2_error") ||
        grepl(
          "UniChem|www[.]ebi[.]ac[.]uk|Timeout|timed out|Failed to perform HTTP request|non-JSON",
          msg,
          ignore.case = TRUE
        )
      if (transient) {
        testthat::skip(paste("UniChem unavailable:", msg))
      }
      stop(e)
    }
  )
}
