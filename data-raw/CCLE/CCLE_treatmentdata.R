# https://data.broadinstitute.org/ccle_legacy_data/pharmacological_profiling/CCLE_NP24.2009_profiling_2012.02.20.csv
filePath <- system.file("extdata/CCLE", "CCLE_NP24.2009_profiling_2012.02.20.csv", package = "AnnotationGx")

rawdata <- data.table::fread(filePath)

CCLE_treatmentMetadata <- 
    rawdata[, .(CCLE.treatmentid = `Compound (code or generic name)`)]


usethis::use_data(CCLE_treatmentMetadata, overwrite = TRUE)
