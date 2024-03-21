library(testthat)
library(AnnotationGx)

test_that("strSplit splits a character vector into a matrix based on a delimiter", {
  input <- "Hello,World"
  expected <- matrix(c("Hello", "World"), ncol = 2, byrow = TRUE)
  result <- strSplit(input, ",")
  expect_equal(result, expected)
})

test_that(".strSplitFinite splits a string into multiple substrings based on a delimiter", {
  input <- "Hello,World,How,Are,You"
  result <- .strSplitFinite(input, ",", 3, fixed = TRUE)
  expected <- c("Hello", "World", "How,Are,You")
  expect_equal(result[[1]], expected)

  result2 <- .strSplitFinite(input, ",", 4, fixed = TRUE)
  expected2 <- c("Hello", "World", "How", "Are,You")
  expect_equal(result2[[1]], expected2)

  result3 <- .strSplitFinite(input, ",", 3, fixed = FALSE)
  expected3 <- c("Hello", "World", "How,Are,You")
  expect_equal(result3[[1]], expected3)
})

test_that(".strSplitInfinite splits a character vector into substrings based on a delimiter", {
  input <- c("apple,banana,orange", "cat,dog,rabbit")
  expected <- list(c("apple", "banana", "orange"), c("cat", "dog", "rabbit"))
  result <- .strSplitInfinite(input, ",", fixed = TRUE)
  expect_equal(result, expected)
})

test_that(".splitCol splits a column into a character list", {
  input <- data.table(col = c("apple;banana", "orange;grape"))
  result <- .splitCol(input, "col", split = ";")
  expected <- data.table(col = list(c("apple", "banana"), c("orange", "grape")))
  expect_equal(result, expected)

  result2 <- .splitCol(input, "col", split = "; ")
  expected2 <- data.table(col = c(list("apple;banana"), list("orange;grape")))
  expect_equal(result2, expected2)
  
})
