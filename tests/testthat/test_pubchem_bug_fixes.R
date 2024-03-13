library(AnnotationGx)
library(testthat)
library(checkmate)


test_that("AnnotationGx::annotatePubchemCompound error from CCLE pipeline",{
    cid <- "60838"
    result <- annotatePubchemCompound(cid, "CAS")

    # Fixed Issue31
    # For now, the fix is to return NA and a warning 
    expect_equal(result, NA_character_)
})
