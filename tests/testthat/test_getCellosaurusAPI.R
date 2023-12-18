library(testthat)


test_that("searchCellosaurusAPI returns expected results", {
  # Test case 1: Search for cell line "MCF-7"
  result1 <- searchCellosaurusAPI("MCF-7")
  expect_equal(nrow(result1), 1)
  expect_true(grepl("MCF-7", result1$Name))

  # Test case 2: Search for cell line "HeLa"
  result2 <- searchCellosaurusAPI("HeLa")
  expect_equal(nrow(result2), 1)
  # expect "HeLa" in the name
  expect_true(grepl("HeLa", result2$Name))
})

test_that("getCellosaurusAccesions returns expected results", {
  # Test case 1: Retrieve Cellosaurus accessions for sample IDs "HeLa" and "22rv1"
  samples <- c("HeLa", "MCF-7")
  result <- getCellosaurusAccesions(samples)
  expect_equal(nrow(result), 2)
  expect_true(grepl("HeLa", result$Name[1]))
  expect_true(grepl("MCF-7", result$Name[2]))
})

test_that("mapCellosaursAccessionsToFields returns expected results", {
  # Test case 1: Map Cellosaurus accessions to fields "id" and "ac"
  accessions <- c("CVCL_0031", "CVCL_0030")
  fields <- c("id", "ac")
  result <- mapCellosaursAccessionsToFields(accessions, fields)
  expect_equal(nrow(result), 2)
  expect_equal(result$Accession, c("CVCL_0031", "CVCL_0030"))
})

