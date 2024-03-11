library(testthat)
library(AnnotationGx)
library(checkmate)

test_that("Valid endpoint returns correct URL", {
  endpoint <- "compounds"
  expected_url <- "https://www.ebi.ac.uk/unichem/api/v1/compounds"
  actual_url <- .build_unichem_query(endpoint)
  expect_equal(actual_url, expected_url)
})

test_that("Invalid endpoint throws an error", {
  endpoint <- "invalid_endpoint"
  expect_error(.build_unichem_query(endpoint))
})

test_that("Query only option returns httr2::httr2_url object", {
  endpoint <- "images"
  query_only <- TRUE
  expected_class <- "httr2_url"
  actual_url <- .build_unichem_query(endpoint, query_only)
  expect_class(actual_url, expected_class)
})


test_that("Valid compound request is built correctly", {
  type <- "uci"
  compound <- "538323"
  expected_url <- "https://www.ebi.ac.uk/unichem/api/v1/compounds"
  expected_body <- list(
    type = type,
    compound = compound
  )
  actual_request <- .build_unichem_compound_req(type, compound)
  expect_equal(actual_request$url, expected_url)
  expect_equal(actual_request$body$data, expected_body)
})

test_that("Valid sourceID compound request is built correctly", {
  type <- "sourceID"
  compound <- "2244"
  sourceID <- 22
  expected_url <- "https://www.ebi.ac.uk/unichem/api/v1/compounds"
  expected_body <- list(
    type = type,
    compound = compound,
    sourceID = sourceID
  )
  actual_request <- .build_unichem_compound_req(type, compound, sourceID)
  expect_equal(actual_request$url, expected_url)
  expect_equal(actual_request$body$data, expected_body)


  response <- actual_request |> 
    .perform_request() |>  
    .parse_resp_json()  

  checkmate::expect_names(
    names(response), 
    subset.of=c("compounds", "notFound", "response", "totalCompounds"))

  checkmate::expect_names(
    names(response$compounds),
    subset.of=c("inchi", "sources", "standardInchiKey", "uci")
  )
  

})

test_that("Invalid type throws an error", {
  type <- "invalid_type"
  compound <- "538323"
  expect_error(.build_unichem_compound_req(type, compound))
})
