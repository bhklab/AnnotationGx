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
#' @usage data(gdsc_sampleMetadata)
#' @examples
#' data(gdsc_sampleMetadata)
#' head(gdsc_sampleMetadata)
#' @source https://www.cancerrxgene.org/
"gdsc_sampleMetadata"




#' cell_model_passports_models is a preprocessed version of the cell model passports dataset
#'
#' A preprocessed version of the cell model passports dataset. This dataset
#' contains the following columns:
#' CMP.model_id, CMP.sampleid, CMP.model_name, CMP.cancer_type_ncit_id,
#' CMP.COSMIC_ID, CMP.BROAD_ID, CMP.CCLE_ID, and CMP.RRID.
#' This dataset is used in the AnnotationGx package to map cell line names from
#' various sources to the Cellosaurus database.
#'
#' @format A data table with 8 columns and 1001 rows.
#' \describe{
#'  \item{CMP.model_id}{`char` The model id of the cell line in the cell model passports dataset.}
#'  \item{CMP.sample_id}{`char` The sample id of the cell line in the cell model passports dataset.}
#'  \item{CMP.model_name}{`char` The name of the cell line in the cell model passports dataset.}
#'  \item{CMP.cancer_type_ncit_id}{`char` The cancer type ncit id of the cell line in the cell model passports dataset.}
#'  \item{CMP.COSMIC_ID}{`int` The COSMIC ID of the cell line in the cell model passports dataset.}
#'  \item{CMP.BROAD_ID}{`int` The BROAD ID of the cell line in the cell model passports dataset.}
#'  \item{CMP.CCLE_ID}{`int` The CCLE ID of the cell line in the cell model passports dataset.}
#'  \item{CMP.RRID}{`char` The RRID of the cell line in the cell model passports dataset.}
#' }
#' @usage data(cell_model_passports_models)
#' @examples
#' data(cell_model_passports_models)
#' head(cell_model_passports_models)
#' @source https://cog.sanger.ac.uk/cmp/download/model_list_20240103.csv
"cell_model_passports_models"