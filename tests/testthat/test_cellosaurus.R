library(AnnotationGx)
library(testthat)
library(checkmate)


test_that("mapCell2Accession works as expected",{
    # Test case 1: Test with a valid cell line name
    cell_line1 <- "Hela"
    expected1 <- "CVCL_0030"
    result1 <- mapCell2Accession(cell_line1,from="id", BPPARAM = BiocParallel::SerialParam(), numResults=1)
    expect_data_table(result1)
    expect_equal(result1$ac, expected1)

    # Test case 2: Test with an invalid cell line name
    cell_line2 <- "InvalidCellLine"
    expected2 <- NA
    result2 <- mapCell2Accession(cell_line2,from="id", BPPARAM = BiocParallel::SerialParam(), numResults=1)
    expect_equal(result2$ac, expected2)


    # Test case 1: Test with a valid cell line name
    cell_line1 <- "hela"
    expected1 <- data.table::data.table(id = "HeLa",ac = "CVCL_0030", query = "id:hela", `query:id` = "hela")
    result1 <- mapCell2Accession(cell_line1,from="id", BPPARAM = BiocParallel::SerialParam(), numResults=1)
    expect_equal(result1, expected1)

    # Test case 2: Test with an invalid cell line name
    cell_line2 <- "InvalidCellLine"
    expected2 <- data.table::data.table(ac = NA, id = NA, query = "id:InvalidCellLine",`query:id` = "InvalidCellLine")
    result2 <- mapCell2Accession(cell_line2,from="id", BPPARAM = BiocParallel::SerialParam(), numResults=1)
    expect_equal(result2, expected2)

})

test_that("mapCell2Accession prioritizePatient works as expected",{

    cell_line <- "BT474"

    result1 <- mapCell2Accession(cell_line,from="id", BPPARAM = BiocParallel::SerialParam(), numResults=1, prioritizeParent = TRUE)
    
    expect_data_table(result1, nrows=1, ncols = 4)
    expect_named(result1,  c("id", "ac", "query", "query:id"))


    # cant prioritizeParent if from != "id".. yet
    expect_error(mapCell2Accession("BT474", numResults=1, from="idsy",  prioritizeParent=T))

})



# Ok read_tsv seems to throw some error on github actions rip
# test_that("mapCell2AccessionWorks on granular level", {

#     data_path <- system.file("extdata", "GDSC_8.4_preprocessed_sampleMetadata.tsv", package = "AnnotationGx")
#     treatment_metadata <- data.table::fread(data_path)

#     # randomly sample 10 cell lines from the treatment metadata
#     cell_lines <- treatment_metadata[["GDSC.sampleid"]] |> unique() |>  sample(10)

#     mapped <- mapCell2Accession(cell_lines)
#     expect_named(mapped)
#     expect_data_table(mapped)
#     expect_equal(names(mapped), c("id", "ac", "query"))


#     cell_lines <- treatment_metadata[["GDSC.BROAD_ID"]] |> unique() |>  sample(10)
# })

