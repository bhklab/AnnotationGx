library(AnnotationGx)
library(testthat)

make_test_mart <- function() {
  AnnotationGx:::MartInfo$new(
    name = "hgnc",
    displayName = "HGNC",
    description = "HGNC gene annotations",
    config = "hgnc_gene_config",
    isHidden = FALSE,
    operation = "SINGLESELECT",
    meta = list(),
    group = "genes"
  )
}

make_test_dataset <- function() {
  AnnotationGx:::DatasetInfo$new(
    name = "hgnc_gene_mart",
    description = "HGNC genes",
    displayName = "HGNC Genes",
    mart = make_test_mart()
  )
}

make_test_attributes <- function() {
  list(
    AnnotationGx:::AttributeInfo$new(
      name = "hgnc_gene__approved_symbol_1010",
      displayName = "Approved symbol"
    ),
    AnnotationGx:::AttributeInfo$new(
      name = "hgnc_gene__name_1010",
      displayName = "Approved name"
    )
  )
}

test_that("BioMart information classes store and print metadata", {
  mart <- make_test_mart()
  dataset <- make_test_dataset()
  filter <- AnnotationGx:::FilterInfo$new(
    name = "hgnc_gene__approved_symbol_1010_text",
    displayName = "Approved symbol",
    type = "text",
    values = c("TP53", "BRCA1"),
    value = "TP53"
  )
  attribute <- make_test_attributes()[[1]]

  expect_identical(mart$name, "hgnc")
  expect_identical(dataset$mart$config, "hgnc_gene_config")
  expect_identical(filter$values, c("TP53", "BRCA1"))
  expect_identical(filter$value, "TP53")
  expect_identical(attribute$displayName, "Approved symbol")

  mart_output <- capture.output(mart_returned <- mart$print(), type = "message")
  dataset_output <- capture.output(
    dataset_returned <- dataset$print(),
    type = "message"
  )
  filter_output <- capture.output(
    filter_returned <- filter$print(),
    type = "message"
  )
  attribute_output <- capture.output(
    attribute_returned <- attribute$print(),
    type = "message"
  )

  expect_identical(mart_returned, mart)
  expect_identical(dataset_returned, dataset)
  expect_identical(filter_returned, filter)
  expect_identical(attribute_returned, attribute)
  expect_match(paste(mart_output, collapse = "\n"), "<MartInfo>")
  expect_match(paste(dataset_output, collapse = "\n"), "<DatasetInfo>")
  expect_match(paste(filter_output, collapse = "\n"), "<FilterInfo>")
  expect_match(paste(attribute_output, collapse = "\n"), "<AttributeInfo>")
})

test_that("AttributeSet selects, filters, and lists attributes", {
  attributes <- AnnotationGx:::AttributeSet$new(make_test_attributes())

  selected <- attributes$get_by_display_name("Approved name")
  included <- attributes$filter("symbol")
  excluded <- attributes$filter("symbol", exclude = TRUE)

  expect_s3_class(selected, "AttributeSet")
  expect_identical(selected$as.list(), "Approved name")
  expect_identical(included$as.list(), "Approved symbol")
  expect_identical(excluded$as.list(), "Approved name")
  expect_identical(
    attributes$as.list(),
    c("Approved symbol", "Approved name")
  )

  output <- capture.output(returned <- attributes$print(), type = "message")
  expect_identical(returned, attributes)
  output <- paste(output, collapse = "\n")
  expect_match(output, "Approved symbol")
  expect_match(output, "Approved name")
})

test_that("BioMartClient normalizes paths and validates metadata objects", {
  client <- BioMartClient$new(
    base_url = "https://biomart.genenames.org/",
    path = "/biomart"
  )

  expect_identical(client$base_url, "https://biomart.genenames.org")
  expect_identical(client$path, "biomart")

  request <- client$.__enclos_env__$private$.request("marts.json")
  expect_s3_class(request, "httr2_request")
  expect_identical(
    request$url,
    "https://biomart.genenames.org/biomart/marts.json"
  )

  expect_error(client$get_datasets(list()))
  expect_error(client$get_attributes(list()))
  expect_error(client$get_filters(list()))
})

test_that("bm_query_builder supports named filters and attribute names", {
  query <- AnnotationGx:::bm_query_builder(
    dataset = make_test_dataset(),
    filters = list(
      hgnc_gene__approved_symbol_1010_text = c("TP53", "BRCA1")
    ),
    attributes = c(
      "hgnc_gene__approved_symbol_1010",
      "hgnc_gene__name_1010"
    ),
    client_name = "AnnotationGx-tests",
    header = TRUE,
    limit = 10
  )

  expect_match(
    query,
    "<Query client='AnnotationGx-tests' processor='TSV' header='1' limit='10'>",
    fixed = TRUE
  )
  expect_match(
    query,
    paste0(
      "<Filter name='hgnc_gene__approved_symbol_1010' ",
      "value='TP53,BRCA1' filter_list=''/>"
    ),
    fixed = TRUE
  )
  expect_match(
    query,
    "<Attribute name='hgnc_gene__approved_symbol_1010'/>",
    fixed = TRUE
  )
  expect_false(grepl("_text", query, fixed = TRUE))
})

test_that("bm_query_builder supports AttributeSet and FilterInfo objects", {
  filter <- AnnotationGx:::FilterInfo$new(
    name = "hgnc_gene__approved_symbol_1010_text",
    value = c("TP53", "BRCA1")
  )
  attributes <- AnnotationGx:::AttributeSet$new(make_test_attributes())

  query <- AnnotationGx:::bm_query_builder(
    dataset = make_test_dataset(),
    filters = list(filter),
    attributes = attributes
  )

  expect_match(
    query,
    paste0(
      "<Filter name='hgnc_gene__approved_symbol_1010' ",
      "value='TP53,BRCA1'/>"
    ),
    fixed = TRUE
  )
  expect_match(
    query,
    "<Attribute name='hgnc_gene__name_1010'/>",
    fixed = TRUE
  )
})

test_that("bm_query_builder rejects invalid inputs", {
  missing_value <- AnnotationGx:::FilterInfo$new(name = "symbol_text")

  expect_error(
    AnnotationGx:::bm_query_builder(
      dataset = make_test_dataset(),
      filters = list(missing_value),
      attributes = "symbol"
    ),
    "Filter 'symbol_text' is missing a value",
    fixed = TRUE
  )
  expect_error(
    AnnotationGx:::bm_query_builder(
      dataset = make_test_dataset(),
      attributes = list("symbol")
    ),
    "attributes must be a character vector or AttributeSet",
    fixed = TRUE
  )
  expect_error(
    AnnotationGx:::bm_query_builder(
      dataset = list(),
      attributes = "symbol"
    )
  )
})

test_that("HGNC BioMart client and query work", {
  skip_if_offline("biomart.genenames.org")

  client <- BioMartClient$new("https://biomart.genenames.org")
  marts <- client$get_marts()
  datasets <- client$get_datasets(marts[[1]])
  attributes <- client$get_attributes(datasets[[1]])
  filters <- client$get_filters(datasets[[1]])

  expect_gt(length(marts), 0)
  expect_s3_class(marts[[1]], "MartInfo")
  expect_gt(length(datasets), 0)
  expect_s3_class(datasets[[1]], "DatasetInfo")
  expect_s3_class(attributes, "AttributeSet")
  expect_gt(length(filters), 0)
  expect_s3_class(filters[[1]], "FilterInfo")

  result <- query_hgnc_by_genes(
    genes = "TP53",
    attributes = c("Approved symbol", "Approved name")
  )

  expect_s3_class(result, "data.table")
  expect_named(result, c("Approved symbol", "Approved name"))
  expect_true("TP53" %in% result[["Approved symbol"]])
})

test_that("query_hgnc_by_genes validates gene and attribute inputs", {
  expect_error(query_hgnc_by_genes(1, "Approved symbol"))
  expect_error(query_hgnc_by_genes("TP53", 1))
})
