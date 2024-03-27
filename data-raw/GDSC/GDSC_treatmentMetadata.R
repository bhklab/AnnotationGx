filePath <- system.file("extdata/GDSC", "GDSC2_8.4_treatmentMetadata.csv", package = "AnnotationGx")
rawdata <- data.table::fread(filePath) 

GDSC_treatmentMetadata <- 
    rawdata[, .(GDSC.treatmentid = `DRUG_NAME`, GDSC.synonyms = `SYNONYMS`, GDSC.drug_id = `DRUG_ID`)]

usethis::use_data(GDSC_treatmentMetadata, overwrite = TRUE)
