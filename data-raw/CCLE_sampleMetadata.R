filePath <- system.file("extdata", "CCLE_Cell_lines_annotations_20181226.txt", package = "AnnotationGx")

rawdata <- data.table::fread(filePath, check.names = T)

CCLE_sampleMetadata <- rawdata[, c("CCLE_ID", "depMapID", "Name")]

# get the first part of the name split on _
CCLE_sampleMetadata$CCLE_ID_parsed <- strsplit(CCLE_sampleMetadata$CCLE_ID, "_") |> 
  purrr::map_chr(1) 

usethis::use_data(CCLE_sampleMetadata, overwrite = TRUE)

