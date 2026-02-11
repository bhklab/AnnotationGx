# Get the list of fields in the Cellosaurus schema

This function retrieves the list of fields available in the Cellosaurus
schema. It internally calls the `.cellosaurus_schema()` function to
fetch the schema and extracts the list of fields from it.

## Usage

``` r
cellosaurus_fields(common = FALSE, upper = FALSE)
```

## Arguments

- common:

  Logical indicating whether to return only the common fields. Default
  is FALSE.

- upper:

  Logical indicating whether to return the fields in uppercase. Default
  is FALSE.

## Value

A character vector containing the list of fields in the Cellosaurus
schema.

## Examples

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
cellosaurus_fields(common = TRUE)
#>  [1] "id"    "ac"    "acas"  "sy"    "dr"    "di"    "din"   "dio"   "ox"   
#> [10] "cc"    "sx"    "ag"    "oi"    "hi"    "ch"    "ca"    "dt"    "dtc"  
#> [19] "dtu"   "dtv"   "from"  "group"
cellosaurus_fields(upper = TRUE)
#>  [1] "ID"                 "SY"                 "IDSY"              
#>  [4] "AC"                 "ACAS"               "DR"                
#>  [7] "REF"                "RX"                 "RA"                
#> [10] "RT"                 "RL"                 "WW"                
#> [13] "GENOME-ANCESTRY"    "HLA"                "REGISTRATION"      
#> [16] "SEQUENCE-VARIATION" "ANECDOTAL"          "BIOTECHNOLOGY"     
#> [19] "BREED"              "CAUTION"            "CELL-TYPE"         
#> [22] "CHARACTERISTICS"    "DONOR-INFO"         "DERIVED-FROM-SITE" 
#> [25] "DISCONTINUED"       "DOUBLING-TIME"      "FROM"              
#> [28] "GROUP"              "KARYOTYPE"          "KNOCKOUT"          
#> [31] "MSI"                "MISCELLANEOUS"      "MISSPELLING"       
#> [34] "MAB-ISOTYPE"        "MAB-TARGET"         "OMICS"             
#> [37] "PART-OF"            "POPULATION"         "PROBLEMATIC"       
#> [40] "RESISTANCE"         "SENESCENCE"         "INTEGRATED"        
#> [43] "TRANSFORMANT"       "VIROLOGY"           "CC"                
#> [46] "STR"                "DI"                 "DIN"               
#> [49] "DIO"                "OX"                 "SX"                
#> [52] "AG"                 "OI"                 "HI"                
#> [55] "CH"                 "CA"                 "DT"                
#> [58] "DTC"                "DTU"                "DTV"               
```
