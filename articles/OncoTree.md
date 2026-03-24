# Querying OncoTree

## Introduction

OncoTree is a standardized classification system used in cancer research
and clinical practice to categorize different types of cancer based on
their tissue of origin, molecular characteristics, and other relevant
factors. Developed by Memorial Sloan Kettering Cancer Center (MSK),
OncoTree provides a hierarchical framework that organizes cancer types
into a structured tree-like diagram.

- provides a standardized classification system for categorizing
  different types of cancer based on their tissue of origin, molecular
  characteristics, and other relevant factors.
- provides a hierarchical framework that organizes cancer types into a
  structured tree-like diagram.
- useful for ensuring consistency in how cancer types are classified and
  reported across different studies and clinical settings.

### Licensing

OncoTree is licensed under [CC BY
4.0](https://creativecommons.org/licenses/by/4.0/). Source:
<https://github.com/cBioPortal/oncotree>

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
#>               code     color
#>             <char>    <char>
#>   1:        PLEURA      Blue
#>   2:      PANCREAS    Purple
#>   3:        CERVIX      Teal
#>   4:          SKIN     Black
#>   5: ADRENAL_GLAND    Purple
#>  ---                        
#> 893:        BIALCL LimeGreen
#> 894:      ALCLALKP LimeGreen
#> 895:      ALCLALKN LimeGreen
#> 896:        PCALCL LimeGreen
#> 897:           LYP LimeGreen
#>                                                          name
#>                                                        <char>
#>   1:                                                   Pleura
#>   2:                                                 Pancreas
#>   3:                                                   Cervix
#>   4:                                                     Skin
#>   5:                                            Adrenal Gland
#>  ---                                                         
#> 893: Breast Implant-Associated Anaplastic Large-Cell Lymphoma
#> 894:              Anaplastic Large-Cell Lymphoma ALK Positive
#> 895:              Anaplastic Large-Cell Lymphoma ALK Negative
#> 896:         Primary Cutaneous Anaplastic Large Cell Lymphoma
#> 897:                                   Lymphomatoid Papulosis
#>                       mainType externalReferences  tissue      children parent
#>                         <char>             <list>  <list>        <char> <char>
#>   1:            Pleural Cancer           C0032225  C12469        Pleura TISSUE
#>   2:         Pancreatic Cancer           C0030274  C12393      Pancreas TISSUE
#>   3:           Cervical Cancer           C0007874  C12311        Cervix TISSUE
#>   4:               Skin Cancer           C1123023  C12470          Skin TISSUE
#>   5:      Adrenal Gland Cancer           C0001625  C12666 Adrenal Gland TISSUE
#>  ---                                                                          
#> 893: Mature T and NK Neoplasms           C4528210 C139012      Lymphoid   ALCL
#> 894: Mature T and NK Neoplasms           C1332079  C37193      Lymphoid   ALCL
#> 895: Mature T and NK Neoplasms           C1332078  C37194      Lymphoid   ALCL
#> 896: Mature T and NK Neoplasms           C1301362   C6860      Lymphoid  PCLPD
#> 897: Mature T and NK Neoplasms           C0206182   C3721      Lymphoid  PCLPD
#>      history level revocations precursors
#>       <list> <int>      <list>     <list>
#>   1:             1                       
#>   2:             1                       
#>   3:             1                       
#>   4:             1                       
#>   5:             1                       
#>  ---                                     
#> 893:             6                       
#> 894:             6                       
#> 895:             6                       
#> 896:             6                       
#> 897:             6
```

``` r
sessionInfo()
#> R version 4.5.3 (2026-03-11)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.3 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] AnnotationGx_0.99.1
#> 
#> loaded via a namespace (and not attached):
#>  [1] cli_3.6.5           knitr_1.51          rlang_1.1.7        
#>  [4] xfun_0.57           textshaping_1.0.5   jsonlite_2.0.0     
#>  [7] data.table_1.18.2.1 glue_1.8.0          backports_1.5.0    
#> [10] htmltools_0.5.9     ragg_1.5.2          sass_0.4.10        
#> [13] rappdirs_0.3.4      rmarkdown_2.30      evaluate_1.0.5     
#> [16] jquerylib_0.1.4     fastmap_1.2.0       yaml_2.3.12        
#> [19] lifecycle_1.0.5     httr2_1.2.2         compiler_4.5.3     
#> [22] fs_2.0.0            systemfonts_1.3.2   digest_0.6.39      
#> [25] R6_2.6.1            curl_7.0.0          magrittr_2.0.4     
#> [28] bslib_0.10.0        checkmate_2.3.4     tools_4.5.3        
#> [31] pkgdown_2.2.0       cachem_1.1.0        desc_1.4.3
```
