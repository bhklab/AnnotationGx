## File: v20.meta.per_compound.txt
## obtained from "https://ctd2-data.nci.nih.gov/Public/Broad/CTRPv2.0_2015_ctd2_ExpandedDataset/CTRPv2.0_2015_ctd2_ExpandedDataset.zip"

# Load the treatment metadata file
filePath <- system.file("extdata", "v20.meta.per_compound.txt", package = "AnnotationGx")
CTRP_treatmentMetadata <- data.table::fread(filePath)[, .(cpd_name, broad_cpd_id)]

# Rename the columns
data.table::setnames(
  CTRP_treatmentMetadata,
  c("cpd_name", "broad_cpd_id"), c("CTRP.treatmentid", "CTRP.broad_cpd_id")
)

# Save the treatment metadata as a data object
usethis::use_data(CTRP_treatmentMetadata, overwrite = TRUE)
