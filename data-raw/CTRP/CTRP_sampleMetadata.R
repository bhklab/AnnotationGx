filePath <- system.file("extdata", "CTRPv2_meta_per_cell_line.txt", package = "AnnotationGx")

rawdata <- data.table::fread(filePath, check.names = T)

CTRP_sampleMetadata <- rawdata[, c("master_ccl_id", "ccl_name")]


usethis::use_data(CTRP_sampleMetadata, overwrite = TRUE)

