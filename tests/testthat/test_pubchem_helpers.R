library(AnnotationGx)
library(testthat)
library(checkmate)


test_that("getPubchemStatus works",{
    output <- capture.output(getPubchemStatus(printMessage = TRUE), type = "message")
    expect_match(output[1], "Throttling status:")
    result <- getPubchemStatus(printMessage = FALSE)
    expect_equal(result, NULL)

    result <- getPubchemStatus(returnMessage = TRUE, printMessage = FALSE)

    expect_class(result, "list")
    expect_equal(names(result), c("request_count", "request_time", "service"))


})

url <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/Aspirin/cids/JSON"
resp_ <- .buildURL(url) |> .build_pubchem_request() |> httr2::req_perform()
test_that("checkThrottlingStatus Works", {
    response <- resp_

    response$headers["X-Throttling-Control"] <- "Request Count status: Yellow (60%), Request Time status: Yellow (60%), Service status: Yellow (60%)"
    parsed_info <- AnnotationGx:::.checkThrottlingStatus2(response, printMessage = FALSE)
    expect_equal(parsed_info, list(request_count = list(status = "Yellow", percent = 60),
                                   request_time = list(status = "Yellow", percent = 60),
                                   service = list(status = "Yellow", percent = 60)))

    response$headers["X-Throttling-Control"] <- 
      "Request Count status: Red (80%), Request Time status: Red (80%), Service status: Red (80%)"
    parsed_info <- AnnotationGx:::.checkThrottlingStatus2(response, printMessage = FALSE)
    expect_equal(parsed_info, list(request_count = list(status = "Red", percent = 80),
                                   request_time = list(status = "Red", percent = 80),
                                   service = list(status = "Red", percent = 80)))

    response$headers["X-Throttling-Control"] <- 
      "Request Count status: Black (100%), Request Time status: Red (80%), Service status: Red (80%)"
    parsed_info <- AnnotationGx:::.checkThrottlingStatus2(response, printMessage = FALSE)
    expect_equal(parsed_info, list(request_count = list(status = "Black", percent = 100),
                                    request_time = list(status = "Red", percent = 80),
                                    service = list(status = "Red", percent = 80)))
})