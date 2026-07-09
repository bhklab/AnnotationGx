# Bundled Data Provenance and Licensing

This file records the provenance of data distributed in the AnnotationGx
source package. The package license covers AnnotationGx code. Upstream data
remain subject to their original source licenses, terms of use, and citation
requirements.

## Runtime Data Distributed in the Package

The example metadata objects are derived from public pharmacogenomic datasets
whose PharmacoSet (PSet) RDS objects are curated, processed, and released by
BHKLab through ORCESTRA/Zenodo. The full RDS PSet objects are not bundled in
AnnotationGx; this package distributes only small, column-reduced metadata
tables used for examples and annotation workflows.

| File or object | Source | License or terms | Notes |
| --- | --- | --- | --- |
| `GDSC_sampleMetadata`, `GDSC_treatmentMetadata` | Genomics of Drug Sensitivity in Cancer (GDSC), <https://www.cancerrxgene.org/>; BHKLab-curated PSet on ORCESTRA: <https://www.orcestra.ca/pset/10.5281/zenodo.3905481> | Public source data; BHKLab-curated/processed PSet released through ORCESTRA/Zenodo. | Derived, column-reduced `data.table` objects used as example metadata. |
| `CCLE_sampleMetadata`, `CCLE_treatmentMetadata` | Cancer Cell Line Encyclopedia and DepMap/Broad resources, including <https://data.broadinstitute.org/ccle_legacy_data/pharmacological_profiling/CCLE_NP24.2009_profiling_2012.02.20.csv>; BHKLab-curated PSet on ORCESTRA: <https://www.orcestra.ca/pset/10.5281/zenodo.3905462> | Public source data; BHKLab-curated/processed PSet released through ORCESTRA/Zenodo. | Derived, column-reduced `data.table` objects used as example metadata. |
| `CTRP_sampleMetadata`, `CTRP_treatmentMetadata` | Cancer Therapeutics Response Portal v2 / NCI CTD2, including <https://ctd2-data.nci.nih.gov/Public/Broad/CTRPv2.0_2015_ctd2_ExpandedDataset/CTRPv2.0_2015_ctd2_ExpandedDataset.zip>; BHKLab-curated PSet on ORCESTRA: <https://www.orcestra.ca/pset/10.5281/zenodo.3905470> | Public source data; BHKLab-curated/processed PSet released through ORCESTRA/Zenodo. | Derived, column-reduced `data.table` objects used as example metadata. |
| `gCSI_sampleMetadata`, `gCSI_treatmentMetadata` | Genentech Cell Line Screening Initiative (gCSI); BHKLab-curated PSet on ORCESTRA: <https://www.orcestra.ca/pset/10.5281/zenodo.4737437> | Public source data generated and shared by Genentech; BHKLab-curated/processed PSet released through ORCESTRA/Zenodo. | Derived, column-reduced `data.table` objects used as example metadata. |
| `inst/extdata/cellosaurus_fields.tsv` | Cellosaurus field definitions, <https://www.cellosaurus.org/> | Cellosaurus is distributed under Creative Commons Attribution 4.0 International (CC BY 4.0), see <https://www.cellosaurus.org/faq>. | Used by `cellosaurus_fields()` and Cellosaurus request validation. |
| `inst/extdata/treatmentMetadata_annotated_pubchem_unichem_chembl.tsv` | AnnotationGx-derived treatment annotation table assembled from package example treatment metadata and PubChem, UniChem, and ChEMBL identifiers | Source treatment metadata derive from public datasets and BHKLab-curated ORCESTRA PSets listed above. PubChem use is subject to NCBI policies, UniChem to EMBL-EBI terms, and ChEMBL to its published license terms. | Used by the annotation standards article, which is not built in the Bioconductor source package. |

## Development Data Excluded from the Source Package

The repository retains some raw source files under `inst/extdata` so that the
data objects can be regenerated during development. These files are excluded
from the built source package through `.Rbuildignore`:

- `inst/extdata/CCLE`
- `inst/extdata/CTRP`
- `inst/extdata/GDSC`
- `inst/extdata/gCSI`
- `inst/extdata/CellModelPassports`
- `inst/extdata/Cell_Lines_Details.xlsx`
- `inst/extdata/bhklabAnnotationFiles`
- `inst/extdata/test_cellosaurus_detailed.R`

The excluded raw files are development inputs. They are not needed at runtime
because the distributed package contains the smaller derived metadata objects
documented above.

## Public Web Services Queried by AnnotationGx

AnnotationGx also provides wrappers around public web services. Query results
are not bundled unless explicitly written into package data.

| Service | Terms or license reference |
| --- | --- |
| PubChem | NCBI Website and Data Usage Policies: <https://www.ncbi.nlm.nih.gov/home/about/policies/> |
| UniChem | EMBL-EBI Terms of Use: <https://www.ebi.ac.uk/about/terms-of-use/> |
| ChEMBL | ChEMBL license and terms: <https://www.ebi.ac.uk/chembl/> |
| Cellosaurus | CC BY 4.0: <https://www.cellosaurus.org/faq> |
| OncoTree | OncoTree license and citation page: <https://oncotree.mskcc.org/#/home?tab=licensing> |
