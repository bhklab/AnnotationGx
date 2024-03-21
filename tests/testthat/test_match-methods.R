library(AnnotationGx)
library(testthat)
library(checkmate)


test_that("unlistNested returns the correct result for a nested list", {
  nested_list <- list(list(1, 2), list(3, 4), list(5, 6))
  expected_result <- c(1, 2, 3, 4, 5, 6)
  expect_equal(unlistNested(nested_list), expected_result)
})

test_that("unlistNested returns the correct result for a nested list with NA values", {
  nested_list <- list(list(1, NA), list(3, 4), list(NA, 6))
  expected_result <- c(1, 3, 4, 6)
  expect_equal(unlistNested(nested_list), expected_result)
})

test_that("unlistNested returns the correct result for a nested list with duplicate values", {
  nested_list <- list(list(1, 2), list(2, 3), list(3, 4))
  expected_result <- c(1, 2, 3, 4)
  expect_equal(unlistNested(nested_list), expected_result)
})

test_that("unlistNested returns an empty vector for an empty nested list", {
  nested_list <- list()
  expected_result <- NULL
  expect_equal(unlistNested(nested_list), expected_result)
})

test_that("unlistNested returns the correct result for a nested list with character elements", {
  nested_list <- list(list("a", "b"), list("c", "d"), list("e", "f"))
  expected_result <- c("a", "b", "c", "d", "e", "f")
  expect_equal(unlistNested(nested_list), expected_result)
})

test_that("unlistNested returns the correct result for a nested list with mixed data types", {
  nested_list <- list(list(1, "a"), list(TRUE, 2.5), list("b", FALSE))
  expected_result <- c(1, "a", TRUE, 2.5, "b", FALSE)
  expect_equal(unlistNested(nested_list), expected_result)
})

test_that("unlistNested returns the correct result for a deeply nested list", {
  nested_list <- list(list(list(1, 2), list(3, 4)), list(list(5, 6), list(7, 8)))
  expected_result <- c(1, 2, 3, 4, 5, 6, 7, 8)
  expect_equal(unlistNested(nested_list), expected_result)
})

test_that("unlistNested returns the correct result for a nested list with NULL values", {
  nested_list <- list(list(NULL, 1), list(2, NULL), list(NULL, NULL))
  expected_result <- c(1, 2)
  expect_equal(unlistNested(nested_list), expected_result)
})

test_that("unlistNested returns the correct result for a nested list with empty sublists", {
  nested_list <- list(list(), list(), list())
  expected_result <- NULL
  expect_equal(unlistNested(nested_list), expected_result)
})

test_that("unlistNested returns the correct result for a nested list with a single element", {
  nested_list <- list(list(42))
  expected_result <- 42
  expect_equal(unlistNested(nested_list), expected_result)
})

####################################################################################################
# matchNested,list
####################################################################################################

test_that("matchNested,list returns the correct index for a nested list", {
  table <- list(list(1, 2), list(3, 4), list(5, 6))
  x <- 3
  expected_result <- 2
  expect_equal(matchNested(x, table), expected_result)
})

test_that("matchNested,list returns the correct index for a nested list", {
  table <- list(list("1", "2"), list("3", "4"), list("5", "6"))
  x <- "3"
  expected_result <- 2
  expect_equal(matchNested(x, table), expected_result)
})


test_that("matchNested,list returns the correct index for a nested list with NA values", {
  table <- list(list(1, NA), list(3, 4), list(NA, 6))
  x <- 4
  expected_result <- 2
  expect_equal(matchNested(x, table), expected_result)
})

test_that("matchNested,list returns the correct index for a nested list with duplicate values", {
  table <- list(list(1, 2), list(2, 3), list(3, 4))
  x <- 2
  expected_result <- 1
  expected_result_dups <- c(1, 2)

  expect_equal(matchNested(x, table, keep_duplicates = FALSE), expected_result)
  expect_equal(matchNested(x, table, keep_duplicates = TRUE), expected_result_dups)

  x <- 4
  expected_result <- 3
  expected_result_dups <- c(3)

  expect_equal(matchNested(x, table), expected_result)
  expect_equal(matchNested(x, table, keep_duplicates = TRUE), expected_result_dups)
})

####################################################################################################
# matchNested,data.table
####################################################################################################
# Test case 1: Matching a character value in a data.table
test_that("matchNested,data.table returns the correct index for a character value", {
  table <- data.table(col1 = c("apple", "banana", "orange"), col2 = c(1, 2, 3))
  x <- "banana"
  expected_result <- 2
  expect_equal(matchNested(x, table), expected_result)
})

# Test case 2: Matching a character value in a data.table with NA values
test_that("matchNested,data.table returns the correct index for a character value with NA values", {
  table <- data.table(col1 = c("apple", NA, "orange"), col2 = c(1, 2, 3))
  x <- "orange"
  expected_result <- 3
  expect_equal(matchNested(x, table), expected_result)
})

# Test case 3: Matching a character value in a data.table with duplicate values
test_that("matchNested,data.table returns the correct index for a character value with duplicate values", {
  table <- data.table(col1 = c("apple", "banana", "banana"), col2 = c(1, 2, 3))
  x <- "banana"
  expected_result <- c(2, 3)
  expect_equal(matchNested(x, table, keep_duplicates = TRUE), expected_result)
  
  expected_result <- 2
  expect_equal(matchNested(x, table, keep_duplicates = FALSE), expected_result)
  expect_equal(matchNested(x, table), expected_result)

  idx <- matchNested(x, table, keep_duplicates = FALSE)

  data.table::setkeyv(table, "col1")
  matched <- table[idx]  

  # make sure that x is in one of the columns
  expect_true(any(matched$col1 == x | matched$col2 == x))
})

test_that("matchNested,data.table returns the correct index for a character value with duplicate values", {
  table <- data.table(
    col1 = list(
      list("apple", "banana"), 
      list("mango", "orange"), 
      list("banana", "orange")), 
    col2 = c(1, "banana", 3)
  )
  x <- "banana"
  expected_result <- c(1, 2, 3)
  expect_equal(matchNested(x, table, keep_duplicates = TRUE), expected_result)

  expected_result <- 1
  expect_equal(matchNested(x, table, keep_duplicates = FALSE), expected_result)

  x <- "orange"
  expected_result <- c(2, 3)
  expect_equal(matchNested(x, table, keep_duplicates = TRUE), expected_result)

  expected_result <- 2
  expect_equal(matchNested(x, table, keep_duplicates = FALSE), expected_result)

})

# Test case 4: Matching a character value in an empty data.table
test_that("matchNested,data.table returns NULL for an empty data.table", {
  table <- data.table()
  x <- "apple"
  expected_result <- NULL
  expect_error(matchNested(x, table))
})

test_that("matchNested returns the correct matches for character and data.frame inputs", {
  # Test case 1: Matching single character with data.frame
  x1 <- "apple"
  table1 <- data.frame(fruit = c("apple", "banana", "orange"), color = c("red", "yellow", "orange"))
  result1 <- matchNested(x1, table1)
  expect_equal(result1, 1)

  # Test case 2: Matching multiple characters with data.frame
  x2 <- c("apple", "banana")
  table2 <- data.frame(fruit = c("apple", "banana", "orange"), color = c("red", "yellow", "orange"))
  expect_warning(result2 <-matchNested(x2, table2))

  expect_equal(result2,  1)


  x3 <- c("apple", "orange")
  expect_warning(result3 <- matchNested(x3, table2))
  expect_equal(result3,  1)

  x4 <- c("red", "yellow")
  expect_warning(result4 <- matchNested(x4, table2))
  expect_equal(result4,  2)
})
