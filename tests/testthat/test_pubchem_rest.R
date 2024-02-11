library(AnnotationGx)
library(testthat)
library(checkmate)


test_that("build_pubchem_rest_query", {
    res <- build_pubchem_rest_query('erlotinib')
    expect_class(res, "httr2_request")

    res2 <- build_pubchem_rest_query('erlotinib', namespace= 'name', operation = 'cids', output = 'JSON')
    expect_class(res2, "httr2_request")

    expect_equal(res , res2)

    res3 <- build_pubchem_rest_query(3672, namespace= 'cid', operation = 'property/InChIKey', output = 'JSON')
    expect_class(res3, "httr2_request")

    res4 <- build_pubchem_rest_query(3672, namespace= 'cid',
        operation = 'property/InChIKey', output = 'JSON', query_only = T)
    expect_class(res4, "character")
})

test_that("build_pubchem_rest_query Failure", {
    expect_error(build_pubchem_rest_query(NA))

    expect_error(build_pubchem_rest_query())

    expect_error(build_pubchem_rest_query(2244, domain='subStance', namespace='cid', operation='record', output='JSON'))

    expect_error(build_pubchem_rest_query(2244, operation='fake'))

    expect_error(build_pubchem_rest_query(1, domain='substance', namespace='cid'))

    expect_error(build_pubchem_rest_query(2244, domain='compound', namespace='cid', operation='Title', output='JSON'))

    expect_error(build_pubchem_rest_query(c("TRETINOIN", "erlotinib", "TRAMETINIB"), domain='compound', namespace='name',
        operation='cids', output='JSON'))

    expect_error(build_pubchem_rest_query(2244, raw = "TRUE"))

    expect_error(build_pubchem_rest_query(2244, query_only = "TRUE"))

    expect_error(build_pubchem_rest_query('test', domain = 'substance', namespace = 'not choice'))

    expect_error(build_pubchem_rest_query('test', domain = 'assay', namespace = 'not choice'))

    expect_error(build_pubchem_rest_query('test', domain = 'cell', namespace = 'not choice'))

    expect_error(build_pubchem_rest_query('test', domain = 'gene', namespace = 'not choice'))

    expect_error(build_pubchem_rest_query('test', domain = 'protein', namespace = 'not choice'))

    lapply(c('TSV', 'PDF', 'XLSX'), function(x) expect_error(build_pubchem_rest_query(2244,  output = x)))

})