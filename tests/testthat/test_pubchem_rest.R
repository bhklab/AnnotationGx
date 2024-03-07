library(AnnotationGx)
library(testthat)
library(checkmate)

compounds <- c('temozolomide', 'erlotinib', 'TRETINOIN', 'TRAMETINIB', 'epigallocatechin-3-monogallate')

# Comprehensive Tests:
test_that("AnnotationGx::getPubchemCompound 5 Correct Drugs", {
    # Test for all possible combinations of domain, namespace, operation, and output
    domains <- c('compound')
    
    expected_cids <- c(5394, 176870, 444795, 11707110, 65064)

    result <- getPubchemCompound(ids = compounds, from = 'name', to = 'cids')
    expect_data_table(
        x = result,
        types = c('character', 'integer'),
        any.missing = FALSE,
        ncols = 2,
        nrows = length(compounds),
        col.names = 'named'
    )
})

test_that("AnnotationGx::getPubchemCompound 1 Incorrect Drug", {
    # Test for an incorrect drug, scoped so it doesnt affect the other tests
    compounds <- c('BAD_DRUG_NAME', compounds)
    getPubchemCompound(ids = compounds, from = 'name', to = 'cids')

    result <- getPubchemCompound('BAD', from='name', to = 'cids', raw = T)[[1]] |> 
    AnnotationGx:::.parse_resp_json() |> 
    AnnotationGx:::.parseQueryToDT()

    expect_data_table(
        x = result,
        types = c('character', 'integer'),
        any.missing = FALSE,
        ncols = 1,
        nrows = 1,
        col.names = 'named'
    )
    
})


# Basic Tests
options::opt_set("log_level", "DEBUG")
test_that("AnnotationGx:::.build_pubchem_rest_query", {
    res <- AnnotationGx:::.build_pubchem_rest_query('erlotinib')
    expect_class(res, "httr2_request")

    res2 <- AnnotationGx:::.build_pubchem_rest_query('erlotinib', namespace= 'name', operation = 'cids', output = 'JSON')
    expect_class(res2, "httr2_request")

    expect_equal(res , res2)

    res3 <- AnnotationGx:::.build_pubchem_rest_query(3672, namespace= 'cid', operation = 'property/InChIKey', output = 'JSON')
    expect_class(res3, "httr2_request")

    res4 <- AnnotationGx:::.build_pubchem_rest_query(3672, namespace= 'cid',
        operation = 'property/InChIKey', output = 'JSON', query_only = T)
    expect_class(res4, "character")
})
options::opt_set("log_level", "WARNING")
test_that("AnnotationGx:::.build_pubchem_rest_query Failure", {
    expect_error(AnnotationGx:::.build_pubchem_rest_query(NA))

    expect_error(AnnotationGx:::.build_pubchem_rest_query())

    expect_error(AnnotationGx:::.build_pubchem_rest_query(2244, domain='subStance', namespace='cid', operation='record', output='JSON'))

    expect_error(AnnotationGx:::.build_pubchem_rest_query(2244, operation='fake'))

    expect_error(AnnotationGx:::.build_pubchem_rest_query(1, domain='substance', namespace='cid'))

    expect_error(AnnotationGx:::.build_pubchem_rest_query(2244, domain='compound', namespace='cid', operation='Title', output='JSON'))

    expect_error(AnnotationGx:::.build_pubchem_rest_query(c("TRETINOIN", "erlotinib", "TRAMETINIB"), domain='compound', namespace='name',
        operation='cids', output='JSON'))

    expect_error(AnnotationGx:::.build_pubchem_rest_query(2244, raw = "TRUE"))

    expect_error(AnnotationGx:::.build_pubchem_rest_query(2244, query_only = "TRUE"))

    expect_error(AnnotationGx:::.build_pubchem_rest_query('test', domain = 'substance', namespace = 'not choice'))

    expect_error(AnnotationGx:::.build_pubchem_rest_query('test', domain = 'assay', namespace = 'not choice'))

    expect_error(AnnotationGx:::.build_pubchem_rest_query('test', domain = 'cell', namespace = 'not choice'))

    expect_error(AnnotationGx:::.build_pubchem_rest_query('test', domain = 'gene', namespace = 'not choice'))

    expect_error(AnnotationGx:::.build_pubchem_rest_query('test', domain = 'protein', namespace = 'not choice'))

    lapply(c('TSV', 'PDF', 'XLSX'), function(x) expect_error(AnnotationGx:::.build_pubchem_rest_query(2244,  output = x)))

})


test_that("getPubchemCompound", {

    result <- getPubchemCompound(2244)
    expect_class(result, "data.table")

    res2 <- getPubchemCompound(c(3672), query_only = T)
    expect_class(res2, "list")

    res3 <- getPubchemCompound(c(3672), raw = T)
    expect_class(res3, "list")
    expect_class(res3[[1]], "httr2_response")

    res4 <- getPubchemCompound('erlotinib', 'name', 'cids')
    expect_class(res4, "data.table")
})


test_that("getPubchemCompound Failure", {

    expect_error(getPubchemCompound(2244, properties = NULL))
    expect_error(getPubchemCompound(2244, properties = c(1234, 1542)))

    }
)