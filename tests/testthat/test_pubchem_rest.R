library(AnnotationGx)
library(testthat)
library(checkmate)

test_that("query_pubchem_rest", {
    res <- query_pubchem_rest('erlotinib')
    expect_class(res, "httr2_response")
    expect_equal(res |> httr2::resp_body_json() |> names(), "IdentifierList")

    res2 <- query_pubchem_rest('erlotinib', namespace= 'name', operation = 'cids', output = 'JSON')
    expect_class(res2, "httr2_response")

    expect_equal(res |> httr2::resp_body_json() , res2 |> httr2::resp_body_json())
})

test_that("query_pubchem_rest Failure", {
    expect_error(query_pubchem_rest(NA))

    expect_error(query_pubchem_rest())

    expect_error(query_pubchem_rest(2244, domain='subStance', namespace='cid', operation='record', output='JSON'))

    expect_error(query_pubchem_rest(2244, operation='fake'))

    expect_error(query_pubchem_rest(1, domain='substance', namespace='cid'))

    expect_error(query_pubchem_rest(2244, domain='compound', namespace='cid', operation='Title', output='JSON'))

    expect_error(query_pubchem_rest(c("TRETINOIN", "erlotinib", "TRAMETINIB"), domain='compound', namespace='name',
        operation='cids', output='JSON'))

    expect_error(query_pubchem_rest(2244, raw = "TRUE"))

    expect_error(query_pubchem_rest(2244, query_only = "TRUE"))

    expect_error(query_pubchem_rest(2244, verbose = "TRUE"))

    expect_error(query_pubchem_rest(2244,  verbose = "TRUE", raw = "TRUE"))

    lapply(c('TSV', 'PDF', 'XLSX'), function(x) expect_error(query_pubchem_rest(2244,  output = x)))

})