# library(AnnotationGx)
# library(testthat)
# library(checkmate)

# test_that("mapCompound2CTD returns expected results", {
#   expect_error(mapCompound2CTD("Invalid Compound", nParallel = 1))
#   # deprecated for now:
#   # # Test case 1: Single compound mapping
#   # compounds <- c("Bax channel blocker")
#   # result <- mapCompound2CTD(compounds, nParallel = 1)
#   # expect_true("displayName" %in% names(result))
#   # expect_true("PUBCHEM" %in% names(result))

#   # result2 <- mapCompound2CTD(compounds, nParallel = 1, query_only = TRUE)
#   # checkmate::assert_list(result2, min.len = 1)
#   # checkmate::assert_class(result2[[1]], "httr2_request")

#   # result3 <- mapCompound2CTD(compounds, nParallel = 1, raw = TRUE)
#   # checkmate::assert_list(result3, min.len = 1)
#   # expect_equal(result3[[1]]$class, "Compound")
#   # expect_equal(result3[[1]]$displayName, "Bax channel blocker")

#   # # Test case 3: Invalid compound mapping
#   # compounds <- c("Invalid Compound")
#   # result4 <- mapCompound2CTD(compounds, nParallel = 1)
#   # expect_true("displayName" %in% names(result4))
#   # checkmate::expect_data_table(result4, min.cols = 1, min.rows = 1)
# })
