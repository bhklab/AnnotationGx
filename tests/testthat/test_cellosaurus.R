library(AnnotationGx)
library(testthat)
library(checkmate)




test_that("mapCell2Accession works as expected", {
  # Test case 1: Test with a valid cell line name
  cell_line1 <- "Hela"
  expected1 <- "CVCL_0030"
  result1 <- mapCell2Accession(cell_line1)
  expect_data_table(result1)
  expect_equal(result1$accession, expected1)

})

test_that("mapCell2Accession prioritizePatient works as expected", {
  cell_line <- "BT474"

  result1 <- mapCell2Accession(
    cell_line)

  expect_data_table(result1, nrows = 1, ncols = 3)
  expect_named(result1, c("cellLineName", "accession", "query"))
  expect_equal(result1$accession, "CVCL_0179")
  expect_equal(result1$cellLineName, "BT-474")

  expect_error(mapCell2Accession("BT474", numResults = -1, from = "idsy"))
})

test_that("mapCell2Accession fuzzy search works as expected", {
  cell_line <- "BT474"

  result1 <- mapCell2Accession(
    cell_line, fuzzy = TRUE)

  expect_data_table(result1, nrows = 1, ncols = 3)
  expect_equal(result1$accession, "CVCL_0179")
  expect_equal(result1$cellLineName, "BT-474")
  expect_named(result1, c("cellLineName", "accession", "query"))

  expect_error(mapCell2Accession("BT474", numResults = -1, from = "idsy"))
})

test_that("mapCell2Accession with multiple cell lines works as expected", {
  cell_lines <- c("BT474", "Hela")

  result1 <- mapCell2Accession(
    cell_lines)

  expect_data_table(result1, nrows = 2, ncols = 3)
  expect_named(result1, c("cellLineName", "accession", "query"))
  expect_equal(result1$accession, c("CVCL_0179", "CVCL_0030"))
  expect_equal(result1$cellLineName, c("BT-474", "HeLa"))

  expect_error(mapCell2Accession("BT474", numResults = -1, from = "idsy"))
})

test_that("mapCell DOR 13 works", {
  name <- "DOR 13"

  result1 <- mapCell2Accession(name)
  result2 <- mapCell2Accession(name, fuzzy = T) 
  result3 <- mapCell2Accession(c(name, "HT"))
  
  expect_data_table(result1, nrows = 1, ncols = 1) # fails
  expect_data_table(result2, nrows = 1, ncols = 3) # works
  expect_data_table(result3, nrows = 2, ncols = 3) # works


  expect_equal(result2$accession, "CVCL_6774")
  expect_equal(result2$cellLineName, "DOV13")

  expect_equal(result3$accession, c(NA_character_, "CVCL_1290"))
  expect_equal(result3$cellLineName, c(NA_character_, "HT"))
  expect_equal(result3$query, c("DOR 13", "HT"))
})


test_that("query only paramater works",{
  result1 <- mapCell2Accession("DOR 13", query_only = TRUE)
  
  expected <- "https://api.cellosaurus.org/search/cell-line?q=idsy%3ADOR%2013&sort=ac%20asc&fields=ac%2Cid%2Csy%2Cmisspelling%2Cdr%2Ccc%2Cca%2Cdi%2Cag%2Csx%2Chi&format=txt&rows=10000"
  expect_equal(result1[[1]], expected)
  expect_equal(names(result1), "DOR 13")
})

test_that("raw param works",{
  
  result1 <- mapCell2Accession("HT", raw = TRUE)
  expect_class(result1[[1]], "httr2_response")
  expect_equal(names(result1), "HT")

  resp <- result1$HT
  lines <- httr2::resp_body_string(resp)  |>
            strsplit("\n") |> 
            unlist()

  checkmate::expect_character(lines)
  expect_true(length(lines) > 2000 & 10000 > length(lines) )


  parsed_lines <- 
    Map(
      f = function(lines, i, j) {
          lines[i:(j - 1L)]
      },
      i = grep(pattern = "^ID\\s+", x = lines, value = FALSE),
      j = grep(pattern = "^//$", x = lines, value = FALSE),
      MoreArgs = list("lines" = lines),
      USE.NAMES = FALSE
    )
  x <- parsed_lines[[1]]
  result <- AnnotationGx:::.processEntry(x)

  expect_data_table(result, min.rows = 1, min.cols = 9)
  expect_true(
    all(
      c("cellLineName", "accession", "comments", "synonyms") %in% colnames(result)
      )
    )

})


test_that("parsed works", {
  ( result1 <- mapCell2Accession("22RV1", parsed = FALSE))$diseases
   expect_data_table(result1, min.rows = 1, min.cols = 10)
  expect_true(
    all(
      c("cellLineName", "accession", "query") %in% colnames(result1)
      )
    )

})
