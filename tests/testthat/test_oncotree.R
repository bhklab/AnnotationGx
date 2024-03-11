library(testthat)
library(AnnotationGx)
library(checkmate)

test_that("Returns data table for versions", {
  result <- AnnotationGx::getOncotreeVersions()
  expect_data_table(
    result,
    ncols = 4,
    min.rows = 25,
    all.missing = FALSE,
  )
})


test_that("Returns data table for main types", {
  result <- AnnotationGx::getOncotreeMainTypes()
  expect_data_table(
    result,
    ncols = 1,
    min.rows = 100,
    all.missing = FALSE,
    col.names = 'named'
  )
})

test_that("Returns data table for tumor types", {
  result <- AnnotationGx::getOncotreeTumorTypes()
  expect_data_table(
    result,
    ncols = 12,
    min.rows = 800,
    all.missing = FALSE,
    col.names = 'named'
  )
})
