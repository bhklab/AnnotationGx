library(AnnotationGx)
library(testthat)
library(checkmate)

# Basic Tests
options("log_level" = "DEBUG")
test_that("AnnotationGx:::.build_pubchem_rest_query", {
  res <- AnnotationGx:::.build_pubchem_rest_query("erlotinib")
  expect_class(res, "httr2_request")

  res2 <- AnnotationGx:::.build_pubchem_rest_query("erlotinib", namespace = "name", operation = "cids", output = "JSON")
  expect_class(res2, "httr2_request")

  expect_equal(res, res2)

  res3 <- AnnotationGx:::.build_pubchem_rest_query(3672, namespace = "cid", operation = "property/InChIKey", output = "JSON")
  expect_class(res3, "httr2_request")

  res4 <- AnnotationGx:::.build_pubchem_rest_query(3672,
    namespace = "cid",
    operation = "property/InChIKey", output = "JSON", query_only = T
  )
  expect_class(res4, "character")
})

options("log_level" = "WARN")

test_that("AnnotationGx:::.build_pubchem_rest_query Failure", {
  expect_error(AnnotationGx:::.build_pubchem_rest_query(NA))

  expect_error(AnnotationGx:::.build_pubchem_rest_query())

  expect_error(AnnotationGx:::.build_pubchem_rest_query(2244, domain = "subStance", namespace = "cid", operation = "record", output = "JSON"))

  expect_error(AnnotationGx:::.build_pubchem_rest_query(2244, operation = "fake"))

  expect_error(AnnotationGx:::.build_pubchem_rest_query(1, domain = "substance", namespace = "cid"))

  expect_error(AnnotationGx:::.build_pubchem_rest_query(2244, domain = "compound", namespace = "cid", operation = "Title", output = "JSON"))

  expect_error(AnnotationGx:::.build_pubchem_rest_query(c("TRETINOIN", "erlotinib", "TRAMETINIB"),
    domain = "compound", namespace = "name",
    operation = "cids", output = "JSON"
  ))

  expect_error(AnnotationGx:::.build_pubchem_rest_query(2244, raw = "TRUE"))

  expect_error(AnnotationGx:::.build_pubchem_rest_query(2244, query_only = "TRUE"))

  expect_error(AnnotationGx:::.build_pubchem_rest_query("test", domain = "substance", namespace = "not choice"))

  expect_error(AnnotationGx:::.build_pubchem_rest_query("test", domain = "assay", namespace = "not choice"))

  expect_error(AnnotationGx:::.build_pubchem_rest_query("test", domain = "cell", namespace = "not choice"))

  expect_error(AnnotationGx:::.build_pubchem_rest_query("test", domain = "gene", namespace = "not choice"))

  expect_error(AnnotationGx:::.build_pubchem_rest_query("test", domain = "protein", namespace = "not choice"))

  lapply(c("TSV", "PDF", "XLSX"), function(x) expect_error(AnnotationGx:::.build_pubchem_rest_query(2244, output = x)))
})
