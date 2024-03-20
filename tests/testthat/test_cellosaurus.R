library(AnnotationGx)
library(testthat)
library(checkmate)


test_that("mapCell2Accession works as expected", {
  # Test case 1: Test with a valid cell line name
  cell_line1 <- "Hela"
  expected1 <- "CVCL_0030"
  result1 <- mapCell2Accession(cell_line1)
  expect_data_table(result1)
  expect_equal(result1$accession, expected1)

})

test_that("mapCell2Accession prioritizePatient works as expected", {
  cell_line <- "BT474"

  result1 <- mapCell2Accession(
    cell_line)

  expect_data_table(result1, nrows = 1, ncols = 3)
  expect_named(result1, c("cellLineName", "accession", "query"))

  expect_error(mapCell2Accession("BT474", numResults = -1, from = "idsy"))
})

