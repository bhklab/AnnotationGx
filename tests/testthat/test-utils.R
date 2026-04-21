library(AnnotationGx)
library(testthat)
library(checkmate)

skip_if_unichem_unavailable <- local({
  checked <- FALSE
  available <- FALSE
  reason <- "UniChem availability has not been checked."

  function() {
    if (!checked) {
      checked <<- TRUE
      available <<- tryCatch(
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
          reason <<- conditionMessage(e)
          FALSE
        }
      )
    }

    if (!available) {
      skip(paste("UniChem unavailable:", reason))
    }
  }
})


test_that(".asDT works", {
  l <- list(Name = c("John", "Doe"), Age = c(1, 2))
  dt <- .asDT(l)
  expect_data_table(dt)
})
