filePath <- system.file("extdata", "gCSI_sampleMap.txt", package = "AnnotationGx")
rawdata <- data.table::fread(filePath, check.names=T)
gCSI_sampleMetadata <- rawdata[,c("Characteristics.cell.line.", "Comment.ENA_SAMPLE.")]

usethis::use_data(gCSI_sampleMetadata, overwrite = TRUE)
