# Querying Cellosaurus

## Introduction

Cellosaurus is a comprehensive knowledge resource dedicated to cell
lines, providing a wealth of information about various types of cells
used in biomedical research. It serves as a centralized repository that
offers detailed data on cell lines, including their origins,
characteristics, authentication methods, references, and more. Please
view the Cellosaurus website at <https://web.expasy.org/cellosaurus/>
for more information and a detailed description can be found at
<https://www.cellosaurus.org/description.html>.

The `AnnotationGx` package provides a wrapper around the Cellosaurus API
to map cell line identifiers to the Cellosaurus database fields.

## Setup

``` r
library(AnnotationGx)
library(data.table)

# set options to warn to quiet info logs
options("log_level" = "WARN")
```

## Mapping from Cell Line name to Accession id

The main function that is provided by the package is
`mapCell2Accession`. This function takes in a vector of cell line
identifiers and returns a `data.table`.

By default, the function will try to map using the common identifiers
and synonyms (`from = "idsy"`) and will return the the Standardized
Identifier as `cellLineName` and the Cellosaurus Accession ID
`accession`. The function also returns an additional column `query`
which can be used to identify the original query if needed.

Let’s see how we can use this function to map the “HeLa” and “A549” cell
line names to the Cellosaurus database.

``` r
mapCell2Accession("hela")
#> [15:05:50][INFO][AnnotationGx::mapCell2Accession] Creating Cellosaurus queries 
#> [15:05:50][INFO][AnnotationGx::mapCell2Accession] Building Cellosaurus requests 
#> [15:05:51][INFO][AnnotationGx::mapCell2Accession] Performing Cellosaurus queries 
#> [15:05:52][INFO][AnnotationGx::mapCell2Accession] Parsing Cellosaurus responses 
#>    cellLineName accession  query
#>          <char>    <char> <char>
#> 1:         HeLa CVCL_0030   hela
```

``` r
mapCell2Accession("A549")
#> [15:05:56][INFO][AnnotationGx::mapCell2Accession] Creating Cellosaurus queries 
#> [15:05:56][INFO][AnnotationGx::mapCell2Accession] Building Cellosaurus requests 
#> [15:05:57][INFO][AnnotationGx::mapCell2Accession] Performing Cellosaurus queries 
#> [15:05:58][INFO][AnnotationGx::mapCell2Accession] Parsing Cellosaurus responses 
#>    cellLineName accession  query
#>          <char>    <char> <char>
#> 1:        A-549 CVCL_0023   A549
```

Functionality for mapping multiple cell lines is also supported.

``` r
mapCell2Accession(c("A549", "THIS SHOULD FAIL", "BT474"))
#> [15:06:02][INFO][AnnotationGx::mapCell2Accession] Creating Cellosaurus queries 
#> [15:06:02][INFO][AnnotationGx::mapCell2Accession] Building Cellosaurus requests 
#> [15:06:03][INFO][AnnotationGx::mapCell2Accession] Performing Cellosaurus queries 
#> Querying Cellosaurus... ■■■■■■■■■■■                       33% | ETA:  4s
#> Querying Cellosaurus... ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> [15:06:06][INFO][AnnotationGx::mapCell2Accession] Parsing Cellosaurus responses 
#> [15:06:09][WARNING]No results found for THIS SHOULD FAIL 
#>    cellLineName accession            query
#>          <char>    <char>           <char>
#> 1:        A-549 CVCL_0023             A549
#> 2:         <NA>      <NA> THIS SHOULD FAIL
#> 3:       BT-474 CVCL_0179            BT474
```

By default, the function will parse the API responses to return the most
common mapping. To return all possible mappings, set `parsed = FALSE`.

``` r
# parsed
mapCell2Accession(c("A549", "hela", "BT474"), parsed = TRUE)
#> [15:06:10][INFO][AnnotationGx::mapCell2Accession] Creating Cellosaurus queries 
#> [15:06:10][INFO][AnnotationGx::mapCell2Accession] Building Cellosaurus requests 
#> [15:06:10][INFO][AnnotationGx::mapCell2Accession] Performing Cellosaurus queries 
#> [15:06:12][INFO][AnnotationGx::mapCell2Accession] Parsing Cellosaurus responses 
#>    cellLineName accession  query
#>          <char>    <char> <char>
#> 1:        A-549 CVCL_0023   A549
#> 2:         HeLa CVCL_0030   hela
#> 3:       BT-474 CVCL_0179  BT474

# no parsing
mapCell2Accession(c("A549", "hela", "BT474"), parsed = FALSE)
#> [15:06:20][INFO][AnnotationGx::mapCell2Accession] Creating Cellosaurus queries 
#> [15:06:20][INFO][AnnotationGx::mapCell2Accession] Building Cellosaurus requests 
#> [15:06:21][INFO][AnnotationGx::mapCell2Accession] Performing Cellosaurus queries 
#> [15:06:22][INFO][AnnotationGx::mapCell2Accession] Parsing Cellosaurus responses 
#>       cellLineName accession         category ageAtSampling sexOfCell
#>             <char>    <char>           <char>        <char>    <char>
#>    1:        A-549 CVCL_0023 Cancer cell line           58Y      Male
#>    2:   A549(VM)28 CVCL_4V06 Cancer cell line           58Y      Male
#>    3:   A549(VP)28 CVCL_4V07 Cancer cell line           58Y      Male
#>    4:  A549.EpoB40 CVCL_4Z15 Cancer cell line           58Y      Male
#>    5:    A549-Dual CVCL_5I73 Cancer cell line           58Y      Male
#>   ---                                                                
#> 4191:  BT474-LAPRa CVCL_EI02 Cancer cell line           60Y    Female
#> 4192:  BT474-LAPRb CVCL_EI03 Cancer cell line           60Y    Female
#> 4193:     BT474-LR CVCL_VL01 Cancer cell line           60Y    Female
#> 4194:     BT474 A3 CVCL_YX79 Cancer cell line           60Y    Female
#> 4195:     BT474-J4 CVCL_ZL46 Cancer cell line           60Y    Female
#>                                                      synonyms  diseases
#>                                                        <list>    <list>
#>    1: A 549,A549,NCI-A549,A549/ATCC,A549 ATCC,A549ATCC,...[7] <list[1]>
#>    2:                                                      NA <list[1]>
#>    3:                                                      NA <list[1]>
#>    4:                                                  EpoB40 <list[1]>
#>    5:                                                      NA <list[1]>
#>   ---                                                                  
#> 4191:                                                      NA <list[1]>
#> 4192:                                                      NA <list[1]>
#> 4193:                                       BT474/LR,BT474 LR <list[1]>
#> 4194:                                                BT474-A3 <list[1]>
#> 4195:                                               BT-474-J4 <list[1]>
#>       crossReferences hierarchy   comments  query
#>                <list>    <list>     <list> <char>
#>    1:    <list[1209]>        NA <list[14]>   A549
#>    2:       <list[2]> <list[1]>  <list[4]>   A549
#>    3:       <list[2]> <list[1]>  <list[4]>   A549
#>    4:       <list[2]> <list[1]>  <list[4]>   A549
#>    5:       <list[3]> <list[1]>  <list[5]>   A549
#>   ---                                            
#> 4191:       <list[2]> <list[1]>  <list[4]>  BT474
#> 4192:       <list[2]> <list[1]>  <list[4]>  BT474
#> 4193:       <list[3]> <list[1]>  <list[4]>  BT474
#> 4194:       <list[2]> <list[1]>  <list[4]>  BT474
#> 4195:      <list[14]> <list[1]>  <list[5]>  BT474
```

### Misspellings and synonyms

The backend of the function also tries to map any misspellings or
synonyms of the cell line names.

``` r

samples <- c("SK23", "SJCRH30")
mapCell2Accession(samples)
#> [15:06:30][INFO][AnnotationGx::mapCell2Accession] Creating Cellosaurus queries 
#> [15:06:30][INFO][AnnotationGx::mapCell2Accession] Building Cellosaurus requests 
#> [15:06:31][INFO][AnnotationGx::mapCell2Accession] Performing Cellosaurus queries 
#> [15:06:32][INFO][AnnotationGx::mapCell2Accession] Parsing Cellosaurus responses 
#>    cellLineName accession   query
#>          <char>    <char>  <char>
#> 1:    SK-MEL-23 CVCL_6027    SK23
#> 2:         Rh30 CVCL_0041 SJCRH30
```

If some cell lines still cannot be found, there is an additional
parameter for fuzzy searching

``` r

# No fuzzy 
mapCell2Accession("DOR 13")
#> [15:06:32][INFO][AnnotationGx::mapCell2Accession] Creating Cellosaurus queries 
#> [15:06:32][INFO][AnnotationGx::mapCell2Accession] Building Cellosaurus requests 
#> [15:06:32][INFO][AnnotationGx::mapCell2Accession] Performing Cellosaurus queries 
#> [15:06:32][INFO][AnnotationGx::mapCell2Accession] Parsing Cellosaurus responses 
#> [15:06:32][WARNING]No results found for DOR 13 
#>     query
#>    <char>
#> 1: DOR 13

# Fuzzy
mapCell2Accession("DOR 13", fuzzy =T)
#> [15:06:32][INFO][AnnotationGx::mapCell2Accession] Creating Cellosaurus queries 
#> [15:06:32][INFO][AnnotationGx::mapCell2Accession] Building Cellosaurus requests 
#> [15:06:32][INFO][AnnotationGx::mapCell2Accession] Performing Cellosaurus queries 
#> [15:06:33][INFO][AnnotationGx::mapCell2Accession] Parsing Cellosaurus responses 
#>    cellLineName accession  query
#>          <char>    <char> <char>
#> 1:        DOV13 CVCL_6774 DOR 13
```

## Annotating Cellosaurus Accessions

Once accession IDs are obtained and the mappings are satisfactory, they
can then be mapped to other fields in the Cellosaurus database. A list
of available fields can be found using
[`cellosaurus_fields()`](https://bhklab.github.io/AnnotationGx/reference/cellosaurus_fields.md)

``` r
cellosaurus_fields()
#>  [1] "id"                 "sy"                 "idsy"              
#>  [4] "ac"                 "acas"               "dr"                
#>  [7] "ref"                "rx"                 "ra"                
#> [10] "rt"                 "rl"                 "ww"                
#> [13] "genome-ancestry"    "hla"                "registration"      
#> [16] "sequence-variation" "anecdotal"          "biotechnology"     
#> [19] "breed"              "caution"            "cell-type"         
#> [22] "characteristics"    "donor-info"         "derived-from-site" 
#> [25] "discontinued"       "doubling-time"      "from"              
#> [28] "group"              "karyotype"          "knockout"          
#> [31] "msi"                "miscellaneous"      "misspelling"       
#> [34] "mab-isotype"        "mab-target"         "omics"             
#> [37] "part-of"            "population"         "problematic"       
#> [40] "resistance"         "senescence"         "integrated"        
#> [43] "transformant"       "virology"           "cc"                
#> [46] "str"                "di"                 "din"               
#> [49] "dio"                "ox"                 "sx"                
#> [52] "ag"                 "oi"                 "hi"                
#> [55] "ch"                 "ca"                 "dt"                
#> [58] "dtc"                "dtu"                "dtv"
```

The
[`annotateCellAccession()`](https://bhklab.github.io/AnnotationGx/reference/annotateCellAccession.md)
function can be used to map the accession IDs to the desired fields. By
default the function will try to map to
`"id", "ac", "hi", "sy", "ca", "sx", "ag", "di", "derived-from-site", "misspelling", "dt"`

``` r

# Annotate the A549 cell line
mappedAccessions <- mapCell2Accession("A549")
#> [15:06:34][INFO][AnnotationGx::mapCell2Accession] Creating Cellosaurus queries 
#> [15:06:34][INFO][AnnotationGx::mapCell2Accession] Building Cellosaurus requests 
#> [15:06:34][INFO][AnnotationGx::mapCell2Accession] Performing Cellosaurus queries 
#> [15:06:35][INFO][AnnotationGx::mapCell2Accession] Parsing Cellosaurus responses 

annotateCellAccession(accessions = mappedAccessions$accession)
#> [15:06:39][INFO][AnnotationGx::annotateCellAccession] Building Cellosaurus requests... 
#> [15:06:39][INFO][AnnotationGx::annotateCellAccession] Performing Requests... 
#> [15:06:40][INFO][AnnotationGx::annotateCellAccession] Parsing Responses... 
#>    cellLineName accession         category
#>          <char>    <char>           <char>
#> 1:        A-549 CVCL_0023 Cancer cell line
#>                                                      date ageAtSampling
#>                                                    <char>        <char>
#> 1: Created: 04-04-12; Last updated: 27-11-25; Version: 53           58Y
#>    sexOfCell                                                synonyms  diseases
#>       <char>                                                  <list>    <list>
#> 1:      Male A 549,A549,NCI-A549,A549/ATCC,A549 ATCC,A549ATCC,...[7] <list[1]>
#>    crossReferences hierarchy  comments
#>             <char>    <char>    <list>
#> 1:            <NA>      <NA> <list[2]>
```
