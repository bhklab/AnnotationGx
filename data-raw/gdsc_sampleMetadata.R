## code to prepare `gdsc_sampleMetadata` dataset goes here
filePath <- system.file("extdata", "Cell_Lines_Details.xlsx", package = "AnnotationGx")
rawdata <- readxl::read_excel(filePath, sheet = 1, col_names = TRUE, na = "NA") |> data.table::as.data.table()

gdsc_sampleMetadata <-
    rawdata[`Sample Name`!= "TOTAL:", .(GDSC.Sample_Name = `Sample Name`, GDSC.COSMIC_ID = `COSMIC identifier`)]

usethis::use_data(gdsc_sampleMetadata, overwrite = TRUE)
