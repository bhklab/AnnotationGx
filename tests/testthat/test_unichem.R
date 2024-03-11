library(testthat)
library(AnnotationGx)
library(checkmate)

test_that("getUnichemSources returns a data.table with the correct columns", {
  sources <- getUnichemSources()
  
  expected_columns <- c(
    "Name", "NameLabel", "NameLong", "SourceID", "CompoundCount", 
    "BaseURL", "URL", "Details", "Description", "ReleaseNumber", 
    "ReleaseDate", "LastUpdated", "UpdateComments"
  )
  
  expect_data_table(
    sources,
    all.missing = FALSE,
    min.rows = 40, # As of March 2024
    min.cols = 13, # As of March 2024
    col.names = 'named',
    info = "The data.table should have the correct columns. 
        The min number of rows and columns may change over time and is set on
        from UniChem as of March 2024.",
    )
})


test_that("queryUnichem returns the expected results", {
  # Test case 1
  result1 <- queryUnichem(type = "sourceID", compound = "444795", sourceID = 22)
  expect_true(is.list(result1))
  expect_true("External_Mappings" %in% names(result1))
  expect_true("UniChem_Mappings" %in% names(result1))
  
  # Test case 2
  expect_error(queryUnichem(type = "inchikey", compound = "InchiKey123"))

})

test_that("queryUnichem returns the expected results 2", {
  # Test case 1
  result1 <- queryUnichem(type = "inchikey", compound = "BSYNRYMUTXBXSQ-UHFFFAOYSA-N", raw = T)

  expect_true(is.list(result1))


  checkmate::expect_names(
    names(result1), 
    subset.of=c("compounds", "notFound", "response", "totalCompounds"))

  checkmate::expect_names(
    names(result1$compounds),
    subset.of=c("inchi", "sources", "standardInchiKey", "uci")
  )

  result2 <- queryUnichem(type = "inchikey", compound = "BSYNRYMUTXBXSQ-UHFFFAOYSA-N", raw = F)

  expect_true(is.list(result2))

  checkmate::expect_names(
    names(result2$External_Mappings),
    subset.of = c("compoundID", "Name", "NameLong", "sourceID", "sourcURL")
  )

  checkmate::expect_names(
    names(result2$UniChem_Mappings),
    subset.of = c(
      "UniChem.UCI", "UniChem.InchiKey", 'UniChem.Inchi',
      'UniChem.formula','UniChem.connections','UniChem.hAtoms'
    )
  )


})