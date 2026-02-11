# Querying OncoTree

## Introduction

OncoTree is a standardized classification system used in cancer research
and clinical practice to categorize different types of cancer based on
their tissue of origin, molecular characteristics, and other relevant
factors. Developed by the National Cancer Institute (NCI) within the
United States, OncoTree provides a hierarchical framework that organizes
cancer types into a structured tree-like diagram.

- provides a standardized classification system for categorizing
  different types of cancer based on their tissue of origin, molecular
  characteristics, and other relevant factors.
- provides a hierarchical framework that organizes cancer types into a
  structured tree-like diagram.
- useful for ensuring consistency in how cancer types are classified and
  reported across different studies and clinical settings.

## Setup

``` r
library(AnnotationGx)
```

## Querying OncoTree

AnnotationGx provides a set of functions for querying OncoTree to
retrieve three types of information: - OncoTree release versions - Main
Cancer types - Subtypes of a specific cancer type and their
relationships

### OncoTree release versions

The `getOncotreeVersions` function retrieves the available OncoTree
release.

``` r
getOncotreeVersions()
#>                 api_identifier
#>                         <char>
#>  1:     oncotree_latest_stable
#>  2:        oncotree_2025_10_03
#>  3:        oncotree_2025_04_08
#>  4:       oncotree_development
#>  5: oncotree_candidate_release
#>  6:        oncotree_2021_11_02
#>  7:        oncotree_2020_10_01
#>  8:        oncotree_2020_04_01
#>  9:        oncotree_2020_02_06
#> 10:        oncotree_2020_02_01
#> 11:        oncotree_2019_12_01
#> 12:        oncotree_2019_08_01
#> 13:        oncotree_2019_05_01
#> 14:        oncotree_2019_03_01
#> 15:        oncotree_2019_02_01
#> 16:        oncotree_2018_11_01
#> 17:        oncotree_2018_09_01
#> 18:        oncotree_2018_08_01
#> 19:        oncotree_2018_07_01
#> 20:        oncotree_2018_06_15
#> 21:        oncotree_2018_06_01
#> 22:        oncotree_2018_05_01
#> 23:        oncotree_2018_04_01
#> 24:        oncotree_2018_03_01
#> 25:        oncotree_2018_02_01
#> 26:        oncotree_2018_01_01
#> 27:        oncotree_2017_11_01
#> 28:        oncotree_2017_10_05
#> 29:        oncotree_2017_06_21
#> 30:        oncotree_legacy_1.1
#>                 api_identifier
#>                         <char>
#>                                                                                                                                                                                                                                                                                                                                            description
#>                                                                                                                                                                                                                                                                                                                                                 <char>
#>  1:                                                                                                                                                                                                                                                                                                This is the latest approved version for public use.
#>  2:                                                                                                                                                                                                                                                                                                        Stable OncoTree released on date 2025-10-03
#>  3:                                                                                                                                                                                                                                                                                                        Stable OncoTree released on date 2025-04-08
#>  4:                                                                                                                                                                                                                                                      Latest OncoTree under development (subject to <b class=text-danger>change without notice</b>)
#>  5: This version of the OncoTree reflects upcoming changes which have been approved for the next public release of oncotree. It also includes a small number of nodes which will not be included in the next public release (see the news page for more details). The next public release may possibly include additional oncotree nodes, if approved.
#>  6:                                                                                                                                                                                                                                                                                                        Stable OncoTree released on date 2021-11-02
#>  7:                                                                                                                                                                                                                                                                                                        Stable OncoTree released on date 2020-10-01
#>  8:                                                                                                                                                                                                                                                                                                        Stable OncoTree released on date 2020-04-01
#>  9:                                                                                                                                                                                                                                                                                                        Stable OncoTree released on date 2020-02-06
#> 10:                                                                                                                                                                                                                                                                                                        Stable OncoTree released on date 2020-02-01
#> 11:                                                                                                                                                                                                                                                                                                        Stable OncoTree released on date 2019-12-01
#> 12:                                                                                                                                                                                                                                                                                                        Stable OncoTree released on date 2019-08-01
#> 13:                                                                                                                                                                                                                                                                                                        Stable OncoTree released on date 2019-05-01
#> 14:                                                                                                                                                                                                                                                                                                        Stable OncoTree released on date 2019-03-01
#> 15:                                                                                                                                                                                                                                                                                                        Stable OncoTree released on date 2019-02-01
#> 16:                                                                                                                                                                                                                                                                                                        Stable OncoTree released on date 2018-11-01
#> 17:                                                                                                                                                                                                                                                                                                        Stable OncoTree released on date 2018-09-01
#> 18:                                                                                                                                                                                                                                                                                                        Stable OncoTree released on date 2018-08-01
#> 19:                                                                                                                                                                                                                                                                                                        Stable OncoTree released on date 2018-07-01
#> 20:                                                                                                                                                                                                                                                                                                        Stable OncoTree released on date 2018-06-15
#> 21:                                                                                                                                                                                                                                                                                                        Stable OncoTree released on date 2018-06-01
#> 22:                                                                                                                                                                                                                                                                                                        Stable OncoTree released on date 2018-05-01
#> 23:                                                                                                                                                                                                                                                                                                        Stable OncoTree released on date 2018-04-01
#> 24:                                                                                                                                                                                                                                                                                                        Stable OncoTree released on date 2018-03-01
#> 25:                                                                                                                                                                                                                                                                                                        Stable OncoTree released on date 2018-02-01
#> 26:                                                                                                                                                                                                                                                                                                        Stable OncoTree released on date 2018-01-01
#> 27:                                                                                                                                                                                                                                                                                                        Stable OncoTree released on date 2017-11-01
#> 28:                                                                                                                                                                                                                                                                                                        Stable OncoTree released on date 2017-10-05
#> 29:                                                                                                                                                                                                                                                                                                        Stable OncoTree released on date 2017-06-21
#> 30:                                                                                                                                                                                                                  This is the closest match in TopBraid for the TumorTypes_txt file associated with release 1.1 of OncoTree (approved by committee)
#>                                                                                                                                                                                                                                                                                                                                            description
#>                                                                                                                                                                                                                                                                                                                                                 <char>
#>     release_date visible
#>           <char>  <lgcl>
#>  1:   2025-10-03    TRUE
#>  2:   2025-10-03   FALSE
#>  3:   2025-04-08   FALSE
#>  4:   2021-11-04    TRUE
#>  5:   2021-11-03    TRUE
#>  6:   2021-11-02   FALSE
#>  7:   2020-10-01   FALSE
#>  8:   2020-04-01   FALSE
#>  9:   2020-02-06   FALSE
#> 10:   2020-02-01   FALSE
#> 11:   2019-12-01   FALSE
#> 12:   2019-08-01   FALSE
#> 13:   2019-05-01   FALSE
#> 14:   2019-03-01   FALSE
#> 15:   2019-02-01   FALSE
#> 16:   2018-11-01   FALSE
#> 17:   2018-09-01   FALSE
#> 18:   2018-08-01   FALSE
#> 19:   2018-07-01   FALSE
#> 20:   2018-06-15   FALSE
#> 21:   2018-06-01   FALSE
#> 22:   2018-05-01   FALSE
#> 23:   2018-04-01   FALSE
#> 24:   2018-03-01   FALSE
#> 25:   2018-02-01   FALSE
#> 26:   2018-01-01   FALSE
#> 27:   2017-11-01   FALSE
#> 28:   2017-10-05   FALSE
#> 29:   2017-06-21   FALSE
#> 30:   2016-03-28   FALSE
#>     release_date visible
#>           <char>  <lgcl>
```

### Main Cancer types

The `getMainCancerTypes` function retrieves the main cancer types in
OncoTree.

``` r
getOncotreeMainTypes()
#>                                            mainType
#>                                              <char>
#>   1:                         Adenocarcinoma In Situ
#>   2: Adenocarcinoma in Retrorectal Cystic Hamartoma
#>   3:                           Adrenal Gland Cancer
#>   4:                         Adrenocortical Adenoma
#>   5:                       Adrenocortical Carcinoma
#>  ---                                               
#> 116:                                Uterine Sarcoma
#> 117:                                 Vaginal Cancer
#> 118:                               Vulvar Carcinoma
#> 119:                          Vulvar/Vaginal Cancer
#> 120:                                    Wilms Tumor
```

### Subtypes of a specific cancer type

The `getCancerSubtypes` function retrieves the subtypes of a specific
cancer type.

``` r
getOncotreeTumorTypes()
#>          code     color
#>        <char>    <char>
#>   1:   BREAST   HotPink
#>   2:   CERVIX      Teal
#>   3:  BLADDER    Yellow
#>   4:    BRAIN      Gray
#>   5:  THYROID      Teal
#>  ---                   
#> 893:      LYP LimeGreen
#> 894:   PCALCL LimeGreen
#> 895: ALCLALKN LimeGreen
#> 896:   BIALCL LimeGreen
#> 897: ALCLALKP LimeGreen
#>                                                          name
#>                                                        <char>
#>   1:                                                   Breast
#>   2:                                                   Cervix
#>   3:                                    Bladder/Urinary Tract
#>   4:                                                CNS/Brain
#>   5:                                                  Thyroid
#>  ---                                                         
#> 893:                                   Lymphomatoid Papulosis
#> 894:         Primary Cutaneous Anaplastic Large Cell Lymphoma
#> 895:              Anaplastic Large-Cell Lymphoma ALK Negative
#> 896: Breast Implant-Associated Anaplastic Large-Cell Lymphoma
#> 897:              Anaplastic Large-Cell Lymphoma ALK Positive
#>                          mainType externalReferences  tissue
#>                            <char>             <list>  <list>
#>   1:                Breast Cancer           C0006141  C12971
#>   2:              Cervical Cancer           C0007874  C12311
#>   3: Bladder/Urinary Tract Cancer           C0005682  C12414
#>   4:             CNS/Brain Cancer           C3714787  C12438
#>   5:               Thyroid Cancer           C0040132  C12400
#>  ---                                                        
#> 893:    Mature T and NK Neoplasms           C0206182   C3721
#> 894:    Mature T and NK Neoplasms           C1301362   C6860
#> 895:    Mature T and NK Neoplasms           C1332078  C37194
#> 896:    Mature T and NK Neoplasms           C4528210 C139012
#> 897:    Mature T and NK Neoplasms           C1332079  C37193
#>                   children parent history level revocations precursors
#>                     <char> <char>  <list> <int>      <list>     <list>
#>   1:                Breast TISSUE             1                       
#>   2:                Cervix TISSUE             1                       
#>   3: Bladder/Urinary Tract TISSUE             1                       
#>   4:             CNS/Brain TISSUE             1                       
#>   5:               Thyroid TISSUE             1                       
#>  ---                                                                  
#> 893:              Lymphoid  PCLPD             6                       
#> 894:              Lymphoid  PCLPD             6                       
#> 895:              Lymphoid   ALCL             6                       
#> 896:              Lymphoid   ALCL             6                       
#> 897:              Lymphoid   ALCL             6
```
