# A Standard for Annotations

``` r
library(AnnotationGx)
```

## Introduction

The goal of AnnotationGx is to provide the tools that may help annotate
chemi- and bio-informatic data. While the package is still in its early
stages, it already provides a number of functions that may be useful for
the annotation of data. In the interest of standardizing the annotation
process, we propose a standard for annotations that may be used in the
future.

## Starting Point

The starting point of any annotation process might be a table with a
number of columns or a list of identifiers that need to be annotated.

For example, we might have a data frame with a column of cell line names
that we would like to annotate with information about the cell lines or
a list of drugs that we would like to annotate with information about
the drugs.

``` r
# "sample" refers to the cell line names
data(CCLE_sampleMetadata)
head(CCLE_sampleMetadata)
#>                                     CCLE_ID   depMapID      Name CCLE_ID_parsed
#>                                      <char>     <char>    <char>         <char>
#> 1:                               DMS53_LUNG ACH-000698    DMS 53          DMS53
#> 2:                   SW1116_LARGE_INTESTINE ACH-000489    SW1116         SW1116
#> 3:                            NCIH1694_LUNG ACH-000431 NCI-H1694       NCIH1694
#> 4: P3HR1_HAEMATOPOIETIC_AND_LYMPHOID_TISSUE ACH-000707    P3HR-1          P3HR1
#> 5: HUT78_HAEMATOPOIETIC_AND_LYMPHOID_TISSUE ACH-000509    HuT 78          HUT78
#> 6:                      UMUC3_URINARY_TRACT ACH-000522   UM-UC-3          UMUC3


# "treatment" refers to the drug names
data(CCLE_treatmentMetadata)
head(CCLE_treatmentMetadata)
#>    CCLE.treatmentid
#>              <char>
#> 1:        Erlotinib
#> 2:        Lapatinib
#> 3:       PHA-665752
#> 4:       PF-2341066
#> 5:           TAE684
#> 6:       Vandetanib
```

### Generic names for classes of data

The first standard is to use generic names for different classes so that
the annotation process can be generalized. When referring to cell lines,
patients, or other biological entities that are being studied, we should
use the name “sample”. When referring to drugs, chemicals, or other
treatments that are being applied to the samples, we should use the name
“treatment”.

**Standard 1**

Use the name “sample” for biological entities and “treatment” for
treatments.

Use the name “treatment” for drugs, chemicals, radiological treatments,
etc that are being applied to the samples.

In the CCLE example data provided, the name of the data frames are
`CCLE_sampleMetadata` and `CCLE_treatmentMetadata`, already following
this standard.

However, within the dataframes, the names of the columns are not
standardized. `CCLE_treatmentMetadata` correctly identifies the column
with the treatment names as “treatmentid”, but `CCLE_sampleMetadata`
uses the varying names.

Before we rename the columns, we introduce the second standard for
column names.

### Standardized column names

Throughout the annotation process, many sources might be used for
generating metadata.

For example, in annotating treatments, one might use the DrugBank
database, the PubChem database, and the ChEMBL database.

For transparency and reproducibility, we should use standardized column
names for the metadata that we collect from these sources.

**Standard 2**

Use the format {SOURCE}.{COLUMN_NAME} for column names in the metadata.

For example, if we are using the Pubchem and DrugBank database, we might
have columns like “pubchem.CID”, “pubchem.SMILES”, “drugbank.ID”,
“drugbank.SMILES”, etc.

This also applies to the data we start with. Take for example the GDSC
example data provided:

``` r
head(GDSC_sampleMetadata)
#>    GDSC.Sample_Name GDSC.COSMIC_ID
#>              <char>          <num>
#> 1:             A253         906794
#> 2:         BB30-HNC         753531
#> 3:         BB49-HNC         753532
#> 4:              BHY         753535
#> 5:           BICR10        1290724
#> 6:           BICR22        1240121
```

The column names above follow both Standard 1 and Standard 2. It tells
us that the data for the two columns comes from the GDSC database.

This is especially useful when we are combining data from multiple
sources and they might have columns with the same name. Additionally, it
provides a level of confidence for users trying to compare data from
different sources.

### Example Annotated Data

In the example below, we have a data frame annotating the treatments for
the 4 datasets CCLE, GDSC, CTRP, and gCSI.

``` r
treatmentMetadata <- data.table::fread(system.file("extdata", "treatmentMetadata_annotated_pubchem_unichem_chembl.tsv", package = "AnnotationGx"))

# two drugs: Erlotinib and Tanespimycin
str(treatmentMetadata[pubchem.CID %in% c("6505803", "176870"),])
#> Classes 'data.table' and 'data.frame':   2 obs. of  26 variables:
#>  $ unichem.ChEMBL                  : chr  "CHEMBL553" "CHEMBL109480"
#>  $ pubchem.CID                     : int  176870 6505803
#>  $ CCLE.treatmentid                : chr  "Erlotinib" "17-AAG"
#>  $ CTRP.treatmentid                : chr  "erlotinib" "tanespimycin"
#>  $ GDSC.treatmentid                : chr  "Erlotinib" "Tanespimycin"
#>  $ gCSI.treatmentid                : chr  "Erlotinib" "17-AAG"
#>  $ pubchem.MolecularFormula        : chr  "C22H23N3O4" "C31H43N3O8"
#>  $ pubchem.MolecularWeight         : num  393 586
#>  $ pubchem.InChIKey                : chr  "AAKJLRGGTJKAMG-UHFFFAOYSA-N" "AYUNIORJHRXIBJ-TXHRRWQRSA-N"
#>  $ pubchem.Title                   : chr  "Erlotinib" "Tanespimycin"
#>  $ unichem.ChEBI                   : chr  "114785" "64153"
#>  $ unichem.FDA_SRS                 : chr  "J4T82NDH7E" "4GY0AVT3L4"
#>  $ unichem.LINCS                   : chr  "LSM-1097" "LSM-43238"
#>  $ unichem.rxnorm                  : chr  "ERLOTINIB HYDROCHLORIDE,TARCEVA,ERLOTINIB" ""
#>  $ unichem.clinicaltrials          : chr  "TARCEVA,OSI-774,CP-358,CP-358,774,CP-358774,ERLOTINIB HYDROCHLORIDE,ERLOTINIB" "KOS-953,BMS-722782,CNF-1010,CNF1010,NSC-330507,17-AAG,TANESPIMYCIN"
#>  $ unichem.DrugBank                : chr  "DB00530" "DB05134"
#>  $ unichem.NIH_Clinical_Collection : chr  "" ""
#>  $ unichem.PharmGKB                : chr  "PA134687924" ""
#>  $ chembl.action_type              : chr  "" "INHIBITOR"
#>  $ chembl.disease_efficacy         : int  NA 1
#>  $ chembl.max_phase                : int  NA 3
#>  $ chembl.mec_id                   : int  NA 5733
#>  $ chembl.mechanism_of_action      : chr  "" "Heat shock protein HSP90 inhibitor"
#>  $ chembl.target_chembl_id         : chr  "" "CHEMBL2095165"
#>  $ chembl.site_id                  : int  NA NA
#>  $ chembl.parent_molecule_chembl_id: chr  "" "CHEMBL109480"
#>  - attr(*, ".internal.selfref")=<externalptr>
```

We can see above how the dataset sources are named in the column names
(‘CCLE.treatmentid’, ‘GDSC.treatmentid’, ‘CTRP.treatmentid’,
‘gCSI.treatmentid’).

If a user wanted to get the InChiKey, they would use the
“pubchem.InChiKey” column, and understand that these inchikeys are from
the PubChem database.

Similarly, they have access to mechanism_of_action data in the
“chembl.mechanism_of_action” column, and understand that these
mechanisms are from the ChEMBL database.
