library(AnnotationGx)
context("Checking getUnichem methods")

test_that("getDatabaseNameToUniChemID Returns correct information", {
  name_to_id <- .getDatabaseNameToUniChemID()
  target_id <- name_to_id[["chembl"]]
  expect_equal(target_id, "1")
})

test_that("InchiToDatabaseID information did not change", {
  inchi <- "AAKJLRGGTJKAMG-UHFFFAOYSA-N"
  database_ids <- inchiToDatabaseID(inchi=inchi)
  expect_equal_to_reference(database_ids, "database_ids.rds")
})

test_that("InchiToDatabaseID specified with target names information did not change",{
  inchi <- "AAKJLRGGTJKAMG-UHFFFAOYSA-N"
  target_names <- c("chembl", "pubchem")
  database_specific_ids <- inchiToDatabaseID(inchi=inchi,
                                             target_names=target_names)
  expect_equal_to_reference(database_specific_ids, "database_specific_ids.rds")
  
})

test_that("wInchiToDatabaseID result did not change", {
  inchi_list <- c("AAKJLRGGTJKAMG-UHFFFAOYSA-N", "AAKJLRGGTJKAMG-UHFFFAOYSA-N")
  target_names <- c("chembl", "pubchem")
  res <- wInchiToDatabaseID(inchi_list, target_names = target_names)
  expect_equal_to_reference(res, "wInchiToDatabaseID_res.rds")
  
})

test_that("identifierToInchikey result did not change", {
  res_two <- identifierToInchikey("CHEMBL12", "chembl")
  expect_equal_to_reference(res_two, "identifierToInchikey_res.rds")

})

test_that("wIdentifierToInchiKey result did not change", {
  ve2 <- c("CHEMBL12", "CHEMBL11")
  expect_equal_to_reference(wIdentifierToInchiKey(ve2, target_names = "chembl"), "wIdentifierToInchiKey_res.rds")
})

test_that("mapBetweenSources result did not change", {
  expect_equal_to_reference(mapBetweenSources("CHEMBL12", "chembl", "pubchem"), "mapBetweenSources_res.rds")
})

test_that("wMapBetweenSources result did not change", {
  ve3 <- c("CHEMBL12", "CHEMBL11")
  expect_equal_to_reference(wMapBetweenSources(ve3, src_name="chembl", target_name="pubchem"), "wMapBetweenSources_res.rds")
})
