library(AnnotationGx)
library(testthat)
library(checkmate)

test_that("build_chembl_request constructs the correct URL", {
  # Set up test data
  resource <- "target"
  field <- "target_chembl_id"
  filter_type <- "exact"
  value <- "CHEMBL2144069"
  format <- "json"

  # Call the function
  url <- AnnotationGx:::.build_chembl_request(resource, field, filter_type, value, format)

  # Check the constructed URL
  expected_url <-"https://www.ebi.ac.uk/chembl/api/data/target?target_chembl_id__exact=CHEMBL2144069&format=json"
  expect_equal(url$url, expected_url)
})


test_that("getChemblMechanism works", {
  # Set up test data
  chembl_id <- "CHEMBL1413"

  # Call the function
  mechanism <- getChemblMechanism(chembl_id)

  # Check the result
  expect_data_table(mechanism)
  expect_equal(nrow(mechanism), 2)
  expect_equal(ncol(mechanism), 17)
  expect_equal(mechanism$target_chembl_id, c("CHEMBL2363058", "CHEMBL2366381"))


  url <- getChemblMechanism(chembl_id, returnURL = T)
  expect_list(url)
  expect_equal(url[[1]], "https://www.ebi.ac.uk/chembl/api/data/mechanism?molecule_chembl_id__in=CHEMBL1413&format=json")
})


test_that("getChemblResourceFields works", {
  mechanism_fields <- getChemblResourceFields("mechanism")

  # should be atomic vector
  expect_character(mechanism_fields)
  # should have 17 elements
  expect_length(mechanism_fields, 17)
  # should contain the expected fields
  expect_equal(mechanism_fields, c(
    "action_type", "binding_site_comment", "direct_interaction", "disease_efficacy",
    "max_phase", "mec_id", "mechanism_comment", "mechanism_of_action",
    "mechanism_refs", "molecular_mechanism", "molecule_chembl_id",
    "parent_molecule_chembl_id", "record_id", "selectivity_comment",
    "site_id", "target_chembl_id", "variant_sequence"
  ))
})

test_that("queryChemblAPI constructs the correct URL and returns parsed JSON response", {
  # Set up test data
  resource <- "mechanism"
  field <- "mechanism_of_action"
  filter_type <- "icontains"
  value <- "Muscarinic acetylcholine receptor"
  format <- "json"
  expected_url <- "https://www.ebi.ac.uk/chembl/api/data/mechanism?mechanism_of_action__icontains=Muscarinic%20acetylcholine%20receptor&format=json"

  request <- AnnotationGx:::.build_chembl_request(resource, field, filter_type, value, format)
  expect_equal(request$url, expected_url)

  # Call the function
  response <- queryChemblAPI(resource, field, filter_type, value, format)

  expect_class(response, "list")

  expect_length(response, 2)
})

test_that("getChemblFilterTypes works", {
  result <- getChemblFilterTypes()

  expect_class(result, "character")
  expect_length(result, 19)

  expect_true("in" %in% result)
})

test_that("getChemblResources works", {
  result <- getChemblResources()

  expect_class(result, "character")
  expect_length(result, 32)

  expect_true("activity" %in% result)
})
