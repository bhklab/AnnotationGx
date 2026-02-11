# Get the Chembl filter types

This function retrieves the Chembl filter types.

## Usage

``` r
getChemblFilterTypes()
```

## Value

A list of Chembl filter types.

## Examples

``` r
getChemblFilterTypes()
#>  [1] "exact"       "iexact"      "contains"    "icontains"   "startswith" 
#>  [6] "istartswith" "endswith"    "iendswith"   "regex"       "iregex"     
#> [11] "gt"          "gte"         "lt"          "lte"         "range"      
#> [16] "in"          "isnull"      "search"      "only"       
```
