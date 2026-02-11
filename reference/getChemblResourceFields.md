# Get the fields of a Chembl resource

This function retrieves the fields of a Chembl resource.

## Usage

``` r
getChemblResourceFields(resource)
```

## Arguments

- resource:

  The Chembl resource.

## Value

A character vector containing the names of the fields.

## Examples

``` r
getChemblResourceFields("molecule")
#>  [1] "atc_classifications"  "availability_type"    "biotherapeutic"      
#>  [4] "black_box_warning"    "chemical_probe"       "chirality"           
#>  [7] "cross_references"     "dosed_ingredient"     "first_approval"      
#> [10] "first_in_class"       "helm_notation"        "inorganic_flag"      
#> [13] "max_phase"            "molecule_chembl_id"   "molecule_hierarchy"  
#> [16] "molecule_properties"  "molecule_structures"  "molecule_synonyms"   
#> [19] "molecule_type"        "natural_product"      "oral"                
#> [22] "orphan"               "parenteral"           "polymer_flag"        
#> [25] "pref_name"            "prodrug"              "score"               
#> [28] "structure_type"       "therapeutic_flag"     "topical"             
#> [31] "usan_stem"            "usan_stem_definition" "usan_substem"        
#> [34] "usan_year"            "veterinary"           "withdrawn_flag"      
```
