library(AnnotationGx)
library(testthat)
library(checkmate)




test_that("AnnotationGx:::.get_all_heading_types", {
    res <- AnnotationGx:::.get_all_heading_types()
    checkmate::expect_data_table(res, min.rows = 1, min.cols = 2, any.missing = FALSE)
    checkmate::expect_names(names(res), must.include = c("Heading", "Type"))
})


test_that("AnnotationGx::getPubchemAnnotationHeadings", {
    query <- getPubchemAnnotationHeadings("compound", "ChEMBL ID")
    expect_data_table(query, ncols = 2, nrows = 1)
    expect_equal(names(query), c("Heading", "Type"))

    dt <- capture.output(
        query <- capture.output(getPubchemAnnotationHeadings("compound", "fake_placeholder"), type = c("message")))
    assert(any(grepl("WARNING", query)))
    expect_equal(dt, "Empty data.table (0 rows and 2 cols): Heading,Type")
})

test_that("AnnotationGx::getAnotationHeadings Failure", {
    expect_error( getPubchemAnnotationHeadings("substance", "ChEMBL ID"))
}
)


test_that("AnnotationGx::annotatePubchemCompound",{
    CID <- 176870   # Erlotonib
    annotatePubchemCompound(CID, 'ChEMBL ID')
    expected <- "CHEMBL553"
    expect_equal(annotatePubchemCompound(CID, 'ChEMBL ID'), expected)

    annotatePubchemCompound(CID, 'CAS')
    expected <- "183321-74-6"
    expect_equal(annotatePubchemCompound(CID, 'CAS'), expected)

    annotatePubchemCompound(CID, 'NSC Number')
    expected <- NULL
    expect_equal(annotatePubchemCompound(CID, 'NSC Number'), expected)

    annotatePubchemCompound(CID, 'ATC Code')
    expected <- "L01EB02"
    expect_equal(annotatePubchemCompound(CID, 'ATC Code'), expected)

    annotatePubchemCompound(CID, 'Drug Induced Liver Injury')
    expected <- "LT01214"
    expect_equal(annotatePubchemCompound(CID, 'Drug Induced Liver Injury'), expected)


    CID <- 3672   # Ibuprofen
    annotatePubchemCompound(CID, 'ChEMBL ID')
    expected <- "CHEMBL521"
    expect_equal(annotatePubchemCompound(CID, 'ChEMBL ID'), expected)

    annotatePubchemCompound(CID, 'CAS')
    expected <- "15687-27-1"
    expect_equal(annotatePubchemCompound(CID, 'CAS'), expected)

    annotatePubchemCompound(CID, 'NSC Number')
    expected <- "NSC 757073; NSC 256857"
    expect_equal(annotatePubchemCompound(CID, 'NSC Number'), expected)

    annotatePubchemCompound(CID, 'ATC Code')
    expected <- "M02AA13; C01EB16; R02AX02; G02CC01; M01AE01"
    expect_equal(annotatePubchemCompound(CID, 'ATC Code'), expected)

    annotatePubchemCompound(CID, 'Drug Induced Liver Injury')
    expected <- "LT00199"
    expect_equal(annotatePubchemCompound(CID, 'Drug Induced Liver Injury'), expected)
})

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

