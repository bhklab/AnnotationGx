library(testthat)
library(AnnotationGx)

test_that("standardize_names converts names to lowercase, removes trailing information, removes non-alphanumeric characters, replaces empty names with 'invalid', and converts names to uppercase", {
  # Test case 1: Standardize names without any special characters
  names1 <- c("John Doe", "Jane Smith", "Alice")
  expected1 <- c("JOHNDOE", "JANESMITH", "ALICE")
  result1 <- standardize_names(names1)
  expect_equal(result1, expected1)

  # Test case 2: Standardize names with trailing information
  names2 <- c("John Doe, Manager", "Jane Smith (Manager)", "Alice, PhD")
  expected2 <- c("JOHNDOE", "JANESMITH", "ALICE")
  result2 <- standardize_names(names2)
  expect_equal(result2, expected2)

  # Test case 3: Standardize names with square brackets and parentheses
  names3 <- c("John Doe [Manager]", "Jane Smith (Manager)", "Alice, PhD")
  expected3 <- c("JOHNDOE", "JANESMITH", "ALICE")
  result3 <- standardize_names(names3)
  expect_equal(result3, expected3)

  # Test case 4: Standardize names with non-alphanumeric characters
  names4 <- c("John@Doe", "Jane-Smith", "Alice123")
  expected4 <- c("JOHNDOE", "JANESMITH", "ALICE123")
  result4 <- standardize_names(names4)
  expect_equal(result4, expected4)

  # Test case 5: Standardize names with empty names
  names5 <- c("John Doe", "", "Alice")
  expected5 <- c("JOHNDOE", NA, "ALICE")
  result5 <- standardize_names(names5)
  expect_equal(result5, expected5)

  # Test case 6: Standardize names with special characters
  names6 <- c("John@Doe", "Jane-Smith", "Alice123")
  expected6 <- c("JOHNDOE", "JANESMITH", "ALICE123")
  result6 <- standardize_names(names6)
  expect_equal(result6, expected6)

  # Test case 7: Standardize names with leading and trailing spaces
  names7 <- c("  John Doe  ", " Jane Smith ", " Alice ")
  expected7 <- c("JOHNDOE", "JANESMITH", "ALICE")
  result7 <- standardize_names(names7)
  expect_equal(result7, expected7)

  # Test case 8: Standardize names with numbers
  names8 <- c("John Doe 1", "Jane Smith 2", "Alice 3")
  expected8 <- c("JOHNDOE1", "JANESMITH2", "ALICE3")
  result8 <- standardize_names(names8)
  expect_equal(result8, expected8)

  # Test case 9: Standardize names with special characters and numbers
  names9 <- c("John@Doe 1", "Jane-Smith 2", "Alice123")
  expected9 <- c("JOHNDOE1", "JANESMITH2", "ALICE123")
  result9 <- standardize_names(names9)
  expect_equal(result9, expected9)

  # Test case 10: Standardize names with non-alphanumeric characters and spaces
  names10 <- c("John@ Doe", "Jane-Smith", "Alice 123")
  expected10 <- c("JOHNDOE", "JANESMITH", "ALICE123")
  result10 <- standardize_names(names10)
  expect_equal(result10, expected10)

  # Test case 11: Standardize names with non-alphanumeric characters and numbers
  names11 <- c("John Doe", 1, "Alice")
  expected11 <- c("JOHNDOE", "1", "ALICE")
  result11 <- standardize_names(names11)
  expect_equal(result6, expected6)
})

test_that("standardize_names Error", {
  names <- c("John Doe", NA, "Alice")
  expect_error(standardize_names(names))

  names <- c(1, 1, 1)
  expect_error(standardize_names(names))
}) # Test case 6: Standardize names with special characters
test_that("cleanCharacterStrings removes special characters, formatting, and unwanted substrings", {
  # Test case 1: Clean string without any special characters
  input1 <- "John Doe"
  expected1 <- "JOHNDOE"
  result1 <- cleanCharacterStrings(input1)
  expect_equal(result1, expected1)

  # Test case 2: Clean string with special characters and formatting
  input2 <- "Cisplatin: 1 mg/mL (1.5 mM); 5 mM in DMSO"
  expected2 <- "CISPLATIN"
  result2 <- cleanCharacterStrings(input2)
  expect_equal(result2, expected2)

  # Test case 3: Clean string with multiple special characters and unwanted substrings
  input3 <- "John@Doe, PhD (Manager)"
  expected3 <- "JOHNDOE"
  result3 <- cleanCharacterStrings(input3)
  expect_equal(result3, expected3)

  # Test case 4: Clean string with unicode characters
  input4 <- "Café"
  expected4 <- "CAFE"
  result4 <- cleanCharacterStrings(input4)
  expect_equal(result4, expected4)

  # Test case 5: Clean string with hyphen
  input5 <- "Bio-informatics"
  expected5 <- "BIOINFORMATICS"
  result5 <- cleanCharacterStrings(input5)
  expect_equal(result5, expected5)
})

test_that("standardize_names handles different scenarios", {
  # Test case 1: Standardize names with trailing information after a comma
  names1 <- c("John Doe, Manager", "Jane Smith, PhD", "Alice")
  expected1 <- c("JOHNDOE", "JANESMITH", "ALICE")
  result1 <- standardize_names(names1)
  expect_equal(result1, expected1)

  # Test case 2: Standardize names with information within square brackets or parentheses
  names2 <- c("John Doe [Manager]", "Jane Smith (Manager)", "Alice, PhD")
  expected2 <- c("JOHNDOE", "JANESMITH", "ALICE")
  result2 <- standardize_names(names2)
  expect_equal(result2, expected2)

  # Test case 3: Standardize names with non-alphanumeric characters
  names3 <- c("John@Doe", "Jane-Smith", "Alice123")
  expected3 <- c("JOHNDOE", "JANESMITH", "ALICE123")
  result3 <- standardize_names(names3)
  expect_equal(result3, expected3)

  # Test case 4: Standardize names with empty names
  names4 <- c("John Doe", "", "Alice")
  expected4 <- c("JOHNDOE", NA, "ALICE")
  result4 <- standardize_names(names4)
  expect_equal(result4, expected4)

  # Test case 5: Standardize names with leading and trailing spaces
  names5 <- c("  John Doe  ", " Jane Smith ", " Alice ")
  expected5 <- c("JOHNDOE", "JANESMITH", "ALICE")
  result5 <- standardize_names(names5)
  expect_equal(result5, expected5)

  # Test case 6: Standardize names with numbers
  names6 <- c("John Doe 1", "Jane Smith 2", "Alice 3")
  expected6 <- c("JOHNDOE1", "JANESMITH2", "ALICE3")
  result6 <- standardize_names(names6)
  expect_equal(result6, expected6)

  # Test case 7: Standardize names with special characters and numbers
  names7 <- c("John@Doe 1", "Jane-Smith 2", "Alice123")
  expected7 <- c("JOHNDOE1", "JANESMITH2", "ALICE123")
  result7 <- standardize_names(names7)
  expect_equal(result7, expected7)

  # Test case 8: Standardize names with non-alphanumeric characters and spaces
  names8 <- c("John@ Doe", "Jane-Smith", "Alice 123")
  expected8 <- c("JOHNDOE", "JANESMITH", "ALICE123")
  result8 <- standardize_names(names8)
  expect_equal(result8, expected8)

  # Test case 9: Standardize names with non-alphanumeric characters and numbers
  names9 <- c("John Doe", 1, "Alice")
  expected9 <- c("JOHNDOE", "1", "ALICE")
  result9 <- standardize_names(names9)
  expect_equal(result9, expected9)
})

test_that("cleanCharacterStrings handles different scenarios", {
  # Test case 1: Clean string without any special characters
  input1 <- "John Doe"
  expected1 <- "JOHNDOE"
  result1 <- cleanCharacterStrings(input1)
  expect_equal(result1, expected1)

  # Test case 2: Clean string with special characters and formatting
  input2 <- "Cisplatin: 1 mg/mL (1.5 mM); 5 mM in DMSO"
  expected2 <- "CISPLATIN"
  result2 <- cleanCharacterStrings(input2)
  expect_equal(result2, expected2)

  # Test case 4: Clean string with unicode characters
  input4 <- "Café"
  expected4 <- "CAFE"
  result4 <- cleanCharacterStrings(input4)
  expect_equal(result4, expected4)

  # Test case 5: Clean string with hyphen
  input5 <- "Bio-informatics"
  expected5 <- "BIOINFORMATICS"
  result5 <- cleanCharacterStrings(input5)
  expect_equal(result5, expected5)

  # Test case 6: Clean numeric input
  input6 <- 12345
  expected6 <- "12345"
  result6 <- cleanCharacterStrings(input6)
  expect_equal(result6, expected6)
})
