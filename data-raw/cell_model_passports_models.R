## code to prepare `cell_model_passports_models` dataset goes here
# https://cog.sanger.ac.uk/cmp/download/model_list_20240103.csv

filePath <- system.file("extdata", "cell_model_passports_list_20240103.csv", package = "AnnotationGx")
cmp_dt <- data.table::fread(filePath)
cols <- c(
  "model_id", "sample_id", "model_name", "cancer_type_ncit_id",
  "COSMIC_ID", "BROAD_ID", "CCLE_ID", "RRID"
)
cell_model_passports_models <- cmp_dt[, ..cols]
names(cell_model_passports_models) <- paste0("CMP.", names(cell_model_passports_models))

usethis::use_data(cell_model_passports_models, overwrite = TRUE)
