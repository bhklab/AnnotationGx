filePath <- system.file("extdata/gCSI", "gCSI_GRmetrics_v1.3.tsv", package = "AnnotationGx")

rawdata <- data.table::fread(filePath, check.names=T)

gCSI_treatmentMetadata <- unique(rawdata[,c("DrugName", "Norm_DrugName")])

data.table::setnames(gCSI_treatmentMetadata, c("DrugName", "Norm_DrugName"), c("gCSI.treatmentid", "gCSI.NormDrugName"))

usethis::use_data(gCSI_treatmentMetadata, overwrite = TRUE)
