library(AnnotationGx)
library(testthat)
library(checkmate)

compounds <- c("temozolomide", "erlotinib", "TRETINOIN", "TRAMETINIB", "epigallocatechin-3-monogallate")

# Comprehensive Tests:
test_that("AnnotationGx::mapCompound2CID 5 Correct Drugs", {
  expected_cids <- c(5394, 176870, 444795, 11707110, 65064)

  result <- mapCompound2CID(names = compounds)
  expect_data_table(
    x = result,
    types = c("character", "integer"),
    any.missing = FALSE,
    ncols = 2,
    nrows = length(compounds),
    col.names = "named"
  )
  result <- mapCompound2CID(names = compounds, first = TRUE)
  expect_data_table(
    x = result,
    types = c("character", "integer"),
    any.missing = FALSE,
    ncols = 2,
    nrows = length(compounds),
    col.names = "named"
  )
})

test_that("AnnotationGx::mapCID2Properties works", {
  result <- mapCID2Properties(ids = c(5394, 176870), properties = c("MolecularWeight", "CanonicalSMILES"))
  expect_data_table(
    x = result,
    types = c("integer", "character", "character"),
    any.missing = FALSE,
    ncols = 3,
    nrows = 2,
    col.names = "named"
  )
})

test_that("getPubchemProperties works", {
  result <- getPubchemProperties()

  expect_data_table(
    x = result,
    types = c("character", "character"),
    any.missing = FALSE,
    ncols = 2,
    min.rows = 45,
    col.names = "named"
  )
})


test_that("AnnotationGx::getPubchemCompound 1 Incorrect Drug", {
  # Test for an incorrect drug, scoped so it doesnt affect the other tests
  compounds <- c("BAD_DRUG_NAME")
  result <- getPubchemCompound(ids = compounds, from = "name", to = "cids")
  expect_data_table(
    x = result,
    types = c("character", "integer"),
    ncols = 2,
    nrows = length(compounds),
    col.names = "named"
  )

  failed_queries <- attributes(result)$failed

  expect_list(
    failed_queries,
    len = 1,
    any.missing = FALSE,
    names = "named"
  )

  expect_equal(names(failed_queries), c("BAD_DRUG_NAME"))
})


test_that("AnnotationGx::getPubchemCompound 2 Incorrect Drugs in a list", {
  # Test for an incorrect drug, scoped so it doesnt affect the other tests
  compounds <- c("BAD_DRUG_NAME", compounds, "Another bad drug")
  result <- getPubchemCompound(ids = compounds, from = "name", to = "cids")
  expect_data_table(
    x = result,
    types = c("character", "integer"),
    ncols = 2,
    nrows = length(compounds),
    col.names = "named"
  )

  failed_queries <- attributes(result)$failed

  expect_list(
    failed_queries,
    len = 2,
    any.missing = FALSE,
    names = "named"
  )

  expect_equal(names(failed_queries), c("BAD_DRUG_NAME", "Another bad drug"))
})

test_that("AnnotationGx::getPubchemCompound errors if cid and not integer", {
  expect_error(
    AnnotationGx::getPubchemCompound(
      ids = c(5394, "PUGREST.BadRequest"),
      from = "cid", to = "property",
      properties = c("Title", "MolecularFormula", "InChIKey", "CanonicalSMILES")
    )
  )
})

