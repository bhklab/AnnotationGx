library(AnnotationGx)
library(testthat)
library(checkmate)


test_that(".asDT works", {
	l <- list(Name = c("John", "Doe"), Age = c(1,2))
	dt <- .asDT(l)
	expect_data_table(dt)
})
