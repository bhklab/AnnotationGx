library(AnnotationGx)
library(testthat)
library(checkmate)


test_that("mapcompound",{
  result <- mapCompound2CID(c("aspirin", "caffeine"))

  expect_data_table(
    x = result,
    types = c("character", "integer"),
    ncols = 2,
    nrows = 2,
    col.names = "named"
  )
})

test_that("mapproperties",{
  props <- mapCID2Properties(ids = c(123, 456), properties = c("MolecularWeight", "CanonicalSMILES"))

  expect_data_table(
    x = props,
    types = c("integer", "character", "character"),
    ncols = 3,
    nrows = 2,
    col.names = "named"
  )
})

test_that("getPubchemCompound", {
  result <- getPubchemCompound(2244)
  expect_class(result, "data.table")

  res2 <- getPubchemCompound(c(3672), query_only = T)
  expect_class(res2, "list")

  res3 <- getPubchemCompound(c(3672), raw = T)
  expect_class(res3, "list")
  expect_class(res3[[1]], "httr2_response")

  res4 <- getPubchemCompound("erlotinib", "name", "cids")
  expect_class(res4, "data.table")
})


test_that("getPubchemCompound Failure", {
  expect_error(getPubchemCompound(2244, properties = NULL))
  expect_error(getPubchemCompound(2244, properties = c(1234, 1542)))
})
