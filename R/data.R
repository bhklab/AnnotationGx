#' gdsc_sampleMetadata is some preprocessed sample metadata from the GDSC dataset
#'
#' A preprocessed version of the sample metadata from the GDSC dataset. This dataset
#' contains the following columns: GDSC.Sample_Name, GDSC.BROAD_ID, GDSC.RRID, GDSC.COSMIC_ID, and CCLE.sampleid.
#' This dataset is used in the AnnotationGx package to map cell line names from various sources to the
#' Cellosaurus database.
#'
#' @format A data table with 5 columns and 1001 rows.
#' \describe{
#'  \item{GDSC.Sample_Name}{`char` The name of the cell line in the GDSC dataset.}
#'  \item{GDSC.COSMIC_ID}{`int` The COSMIC ID of the cell line in the GDSC dataset.}
#' }
#' @usage data(GDSC_sampleMetadata)
#' @examples
#' data(GDSC_sampleMetadata)
#' head(GDSC_sampleMetadata)
#' @source https://www.cancerrxgene.org/
"GDSC_sampleMetadata"


#' CCLE_sampleMetadata is some preprocessed sample metadata from the CCLE dataset
#' 
"CCLE_sampleMetadata"

#' CTRP_sampleMetadata is some preprocessed sample metadata from the CTRP dataset
#' 
"CTRP_sampleMetadata"

#' gCSI_sampleMetadata is some preprocessed sample metadata from the NCI60 dataset
#' 
"gCSI_sampleMetadata"
