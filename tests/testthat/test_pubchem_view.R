library(AnnotationGx)
library(testthat)
library(checkmate)




test_that("AnnotationGx:::.get_all_heading_types", {
    res <- AnnotationGx:::.get_all_heading_types()
    checkmate::expect_data_table(res, min.rows = 1, min.cols = 2, any.missing = FALSE)
    checkmate::expect_names(names(res), must.include = c("Heading", "Type"))
})


test_that("AnnotationGx::getAnnotationHeadings", {
    query <- getAnnotationHeadings("compound", "ChEMBL ID")
    expect_data_table(query, ncols = 2, nrows = 1)
    expect_equal(names(query), c("Heading", "Type"))

    dt <- capture.output(
        query <- capture.output(getAnnotationHeadings("compound", "fake_placeholder"), type = c("message")))
    assert(any(grepl("WARNING", query)))
    expect_equal(dt, "Empty data.table (0 rows and 2 cols): Heading,Type")
})

test_that("AnnotationGx::getAnotationHeadings Failure", {
    expect_error( getAnnotationHeadings("substance", "ChEMBL ID"))
}
)

test_that("AnnotationGx:::.build_pubchem_view_query", {
  # Test case 1: Test with default parameters
    query <- AnnotationGx:::.build_pubchem_view_query(id = "12345")
    expected_url <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/12345/JSON"
    expect_equal(query, expected_url)

    # Test case 2: Test with custom parameters
    query <- AnnotationGx:::.build_pubchem_view_query(
    id = "67890", record = "substance", page = 2)
    expected_url <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/substance/67890/JSON?page=2"
    expect_equal(query, expected_url)

    query <- AnnotationGx:::.build_pubchem_view_query(
    id = "176870", heading = "ChEMBL ID", output = "XML")
    expected_url <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/176870/XML?heading=ChEMBL%20ID"
    expect_equal(query, expected_url)

    query <- AnnotationGx:::.build_pubchem_view_query(
    id = "176870",output = "JSON", source = "DrugBank")
    expected_url <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/176870/JSON?source=DrugBank"
    expect_equal(query, expected_url)

    query <- AnnotationGx:::.build_pubchem_view_query(
        id = "176870", record = "substance", version = '1.2')
    expected_url <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/substance/176870/JSON?version=1.2"
    expect_equal(query, expected_url)

    query <- AnnotationGx:::.build_pubchem_view_query(
        id = "176870", version = 1)
    expected_url <- "https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/176870/JSON?version=1"
    expect_equal(query, expected_url)
})


test_that("AnnotationGx:::.build_pubchem_view_query Failure", {
  # Test case 1: Test with invalid annotation
  expect_error(AnnotationGx:::.build_pubchem_view_query(id = "67890", record = "substance",
    page = 2, version = 1, heading = "Heading1", source = "Source1", output = "XML"))
    expect_error(AnnotationGx:::.build_pubchem_view_query(id = "67890", record ="substance",  version = 1.5))
    expect_error(AnnotationGx:::.build_pubchem_view_query(
        id = "176870", record = "substance", version = 1))

    expect_error(AnnotationGx:::.build_pubchem_view_query(
        id = "176870",output = "JSON", source = ""))
})

