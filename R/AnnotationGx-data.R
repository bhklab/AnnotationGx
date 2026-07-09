#' GDSC Sample Metadata
#'
#' A column-reduced example sample metadata table derived from the Genomics of
#' Drug Sensitivity in Cancer (GDSC) data.
#'
#' @format A `data.table` with 1001 rows and 2 columns:
#' \describe{
#'   \item{GDSC.Sample_Name}{GDSC cell line name.}
#'   \item{GDSC.COSMIC_ID}{COSMIC identifier.}
#' }
#' @source GDSC, <https://www.cancerrxgene.org/>; BHKLab-curated PSet on
#' ORCESTRA, <https://www.orcestra.ca/pset/10.5281/zenodo.3905481>. See
#' `system.file("extdata", "LICENSES.md", package = "AnnotationGx")` for
#' bundled data provenance notes.
#' @usage data(GDSC_sampleMetadata)
#' @examples
#' data(GDSC_sampleMetadata)
#' head(GDSC_sampleMetadata)
"GDSC_sampleMetadata"

#' GDSC Treatment Metadata
#'
#' A column-reduced example treatment metadata table derived from GDSC.
#'
#' @format A `data.table` with 621 rows and 3 columns:
#' \describe{
#'   \item{GDSC.treatmentid}{GDSC treatment name.}
#'   \item{GDSC.synonyms}{GDSC-provided treatment synonyms.}
#'   \item{GDSC.drug_id}{GDSC drug identifier.}
#' }
#' @source GDSC, <https://www.cancerrxgene.org/>; BHKLab-curated PSet on
#' ORCESTRA, <https://www.orcestra.ca/pset/10.5281/zenodo.3905481>. See
#' `system.file("extdata", "LICENSES.md", package = "AnnotationGx")` for
#' bundled data provenance notes.
#' @usage data(GDSC_treatmentMetadata)
#' @examples
#' data(GDSC_treatmentMetadata)
#' head(GDSC_treatmentMetadata)
"GDSC_treatmentMetadata"

#' CCLE Sample Metadata
#'
#' A column-reduced example sample metadata table derived from Cancer Cell Line
#' Encyclopedia/DepMap resources.
#'
#' @format A `data.table` with 1461 rows and 4 columns:
#' \describe{
#'   \item{CCLE_ID}{CCLE cell line identifier.}
#'   \item{depMapID}{DepMap identifier.}
#'   \item{Name}{Cell line display name.}
#'   \item{CCLE_ID_parsed}{Parsed cell line identifier used by examples.}
#' }
#' @source CCLE/DepMap resources; BHKLab-curated PSet on ORCESTRA,
#' <https://www.orcestra.ca/pset/10.5281/zenodo.3905462>. See
#' `system.file("extdata", "LICENSES.md", package = "AnnotationGx")` for
#' bundled data provenance notes.
#' @usage data(CCLE_sampleMetadata)
#' @examples
#' data(CCLE_sampleMetadata)
#' head(CCLE_sampleMetadata)
"CCLE_sampleMetadata"

#' CCLE Treatment Metadata
#'
#' A column-reduced example treatment metadata table derived from legacy CCLE
#' pharmacological profiling data.
#'
#' @format A `data.table` with 24 rows and 1 column:
#' \describe{
#'   \item{CCLE.treatmentid}{CCLE treatment name.}
#' }
#' @source CCLE legacy pharmacological profiling data,
#' <https://data.broadinstitute.org/ccle_legacy_data/pharmacological_profiling/CCLE_NP24.2009_profiling_2012.02.20.csv>;
#' BHKLab-curated PSet on ORCESTRA,
#' <https://www.orcestra.ca/pset/10.5281/zenodo.3905462>. See
#' `system.file("extdata", "LICENSES.md", package = "AnnotationGx")` for
#' bundled data provenance notes.
#' @usage data(CCLE_treatmentMetadata)
#' @examples
#' data(CCLE_treatmentMetadata)
#' head(CCLE_treatmentMetadata)
"CCLE_treatmentMetadata"

#' CTRP Sample Metadata
#'
#' A column-reduced example sample metadata table derived from Cancer
#' Therapeutics Response Portal v2 metadata.
#'
#' @format A `data.table` with 1107 rows and 2 columns:
#' \describe{
#'   \item{master_ccl_id}{CTRP master cell line identifier.}
#'   \item{ccl_name}{CTRP cell line name.}
#' }
#' @source Cancer Therapeutics Response Portal v2 / NCI CTD2,
#' <https://ctd2-data.nci.nih.gov/Public/Broad/CTRPv2.0_2015_ctd2_ExpandedDataset/CTRPv2.0_2015_ctd2_ExpandedDataset.zip>;
#' BHKLab-curated PSet on ORCESTRA,
#' <https://www.orcestra.ca/pset/10.5281/zenodo.3905470>. See
#' `system.file("extdata", "LICENSES.md", package = "AnnotationGx")` for
#' bundled data provenance notes.
#' @usage data(CTRP_sampleMetadata)
#' @examples
#' data(CTRP_sampleMetadata)
#' head(CTRP_sampleMetadata)
"CTRP_sampleMetadata"

#' CTRP Treatment Metadata
#'
#' A column-reduced example treatment metadata table derived from Cancer
#' Therapeutics Response Portal v2 metadata.
#'
#' @format A `data.table` with 545 rows and 2 columns:
#' \describe{
#'   \item{CTRP.treatmentid}{CTRP compound name.}
#'   \item{CTRP.broad_cpd_id}{Broad compound identifier.}
#' }
#' @source Cancer Therapeutics Response Portal v2 / NCI CTD2,
#' <https://ctd2-data.nci.nih.gov/Public/Broad/CTRPv2.0_2015_ctd2_ExpandedDataset/CTRPv2.0_2015_ctd2_ExpandedDataset.zip>;
#' BHKLab-curated PSet on ORCESTRA,
#' <https://www.orcestra.ca/pset/10.5281/zenodo.3905470>. See
#' `system.file("extdata", "LICENSES.md", package = "AnnotationGx")` for
#' bundled data provenance notes.
#' @usage data(CTRP_treatmentMetadata)
#' @examples
#' data(CTRP_treatmentMetadata)
#' head(CTRP_treatmentMetadata)
"CTRP_treatmentMetadata"

#' gCSI Sample Metadata
#'
#' A column-reduced example sample metadata table derived from Genentech Cell
#' Line Screening Initiative (gCSI) metadata.
#'
#' @format A `data.table` with 1350 rows and 2 columns:
#' \describe{
#'   \item{Characteristics.cell.line.}{gCSI cell line name.}
#'   \item{Comment.ENA_SAMPLE.}{ENA sample accession.}
#' }
#' @source gCSI source metadata; BHKLab-curated PSet on ORCESTRA,
#' <https://www.orcestra.ca/pset/10.5281/zenodo.4737437>. See
#' `system.file("extdata", "LICENSES.md", package = "AnnotationGx")` for
#' bundled data provenance notes.
#' @usage data(gCSI_sampleMetadata)
#' @examples
#' data(gCSI_sampleMetadata)
#' head(gCSI_sampleMetadata)
"gCSI_sampleMetadata"

#' gCSI Treatment Metadata
#'
#' A column-reduced example treatment metadata table derived from gCSI response
#' metadata.
#'
#' @format A `data.table` with 44 rows and 2 columns:
#' \describe{
#'   \item{gCSI.treatmentid}{gCSI treatment name.}
#'   \item{gCSI.NormDrugName}{Normalized gCSI treatment name.}
#' }
#' @source gCSI source metadata; BHKLab-curated PSet on ORCESTRA,
#' <https://www.orcestra.ca/pset/10.5281/zenodo.4737437>. See
#' `system.file("extdata", "LICENSES.md", package = "AnnotationGx")` for
#' bundled data provenance notes.
#' @usage data(gCSI_treatmentMetadata)
#' @examples
#' data(gCSI_treatmentMetadata)
#' head(gCSI_treatmentMetadata)
"gCSI_treatmentMetadata"
