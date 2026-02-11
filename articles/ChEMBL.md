# Querying ChEMBL Database

## Introduction to ChEMBL API

**WARNING: This vignette is a work in progress. If you have questions or
would like to see more features, please open an issue at
[bhklab/AnnotationGx](https://github.com/bhklab/AnnotationGx)**

The ChEMBL database contains information on bioactive drug-like small
molecules. The information includes 2-D structures, calculated
properties; logP, Molecular Weight, Lipinski Parameters, and abstracted
bioactivities; binding constants and ADMET data. The data is curated
from primary scientific literature. The ChEMBL API allows for the data
to be made available for retrieval in a programmatic fashion. We can use
the API to query CHEMBL ID of a compound, retrieve all molecule
mechanisms of action, query compound_record resource and molecule
resource from the ChEMBL database.

### Setup

``` r
library(AnnotationGx)
```

### Retrieve molecule mechanisms of action from ChEMBL

Given a ChEMBL ID, we can retrieve the molecule mechanisms of action
from the ChEMBL database using the
[`getChemblMechanism()`](https://bhklab.github.io/AnnotationGx/reference/getChemblMechanism.md)
function.

**NOTE: This is a specialized function that queries the API for the
*mechanism* resource only. To query other resources, please see the
[Custom Queries](#custom-queries) section.**

``` r
mechs <- getChemblMechanism("CHEMBL1413")
mechs
#>        action_type binding_site_comment direct_interaction disease_efficacy
#>             <char>               <lgcl>              <int>            <int>
#> 1: CHELATING AGENT                   NA                  1                1
#> 2: CHELATING AGENT                   NA                  1                1
#>    max_phase mec_id
#>        <int>  <int>
#> 1:         4   2200
#> 2:         4   2224
#>                                                                                                                                                  mechanism_comment
#>                                                                                                                                                             <char>
#> 1: Trivalent metal cations chelating agent; inhibition of the metal-dependent enzymes that are responsible for the degradation of peroxides within the fungal cell
#> 2: Trivalent metal cations chelating agent; inhibition of the metal-dependent enzymes that are responsible for the degradation of peroxides within the fungal cell
#>          mechanism_of_action    mechanism_refs molecular_mechanism
#>                       <char>            <list>               <int>
#> 1:      Iron chelating agent <data.frame[3x3]>                   1
#> 2: Aluminium chelating agent <data.frame[3x3]>                   1
#>    molecule_chembl_id parent_molecule_chembl_id record_id selectivity_comment
#>                <char>                    <char>     <int>              <lgcl>
#> 1:         CHEMBL1413                CHEMBL1413   1343970                  NA
#> 2:         CHEMBL1413                CHEMBL1413   1343970                  NA
#>    site_id target_chembl_id variant_sequence
#>     <lgcl>           <char>           <lgcl>
#> 1:      NA    CHEMBL2363058               NA
#> 2:      NA    CHEMBL2366381               NA
```

In the above example, multiple mechanisms of action are returned.

### Custom Queries

The ChEMBL API allows for a wide range of queries. We have specialized
one function, but are open to incorporating more. Please open an issue
at [bhklab/AnnotationGx](https://github.com/bhklab/AnnotationGx) with an
idea of a specialized function that meets a use case.

A query to the API follows the following format:

    https://www.ebi.ac.uk/chembl/api/data/[resource]?[field]__[filter_type]=[value]&format=[format]

More information can be found at the [API
Documentation](https://chembl.gitbook.io/chembl-interface-documentation/web-services/chembl-data-web-**services******)

In summary, the requirements for a query are:

1.  The `resource` to be queried
2.  The reource `field` to be queried
3.  The `filter_type` to be used
4.  The `value` to be used for the filter
5.  (optional) The `format` of the returned data (default is JSON)

For example, the query for the example in [the above
section](#chembl-mechanisms) would be:
“<https://www.ebi.ac.uk/chembl/api/data/mechanism?molecule_chembl_id__in=CHEMBL1413&format=json>”
where:

- `resource` is “mechanism”
- `field` is “molecule_chembl_id”
- `filter_type` is “in”
- `value` is “CHEMBL1413”
- `format` is “json”

These parameters can be used in the
`queryChemblAPI(resource, field, filter_type, value, format = "json")`
function to query the ChEMBL API.

**NOTE: unlike the
[`getChemblMechanism()`](https://bhklab.github.io/AnnotationGx/reference/getChemblMechanism.md)
function which returns a `data.table`, the
[`queryChemblAPI()`](https://bhklab.github.io/AnnotationGx/reference/queryChemblAPI.md)
function returns the raw data unformatted**

``` r
queryChemblAPI("mechanism", "molecule_chembl_id", "in", "CHEMBL1413")
#> $mechanisms
#>       action_type binding_site_comment direct_interaction disease_efficacy
#> 1 CHELATING AGENT                   NA                  1                1
#> 2 CHELATING AGENT                   NA                  1                1
#>   max_phase mec_id
#> 1         4   2200
#> 2         4   2224
#>                                                                                                                                                 mechanism_comment
#> 1 Trivalent metal cations chelating agent; inhibition of the metal-dependent enzymes that are responsible for the degradation of peroxides within the fungal cell
#> 2 Trivalent metal cations chelating agent; inhibition of the metal-dependent enzymes that are responsible for the degradation of peroxides within the fungal cell
#>         mechanism_of_action
#> 1      Iron chelating agent
#> 2 Aluminium chelating agent
#>                                                                                                                                                                                                                                 mechanism_refs
#> 1 20964457, 23416050, Ciclopirox#cite_note-pmid12760852-4, PubMed, PubMed, Wikipedia, http://europepmc.org/abstract/MED/20964457, http://europepmc.org/abstract/MED/23416050, http://en.wikipedia.org/wiki/Ciclopirox#cite_note-pmid12760852-4
#> 2 20964457, 23416050, Ciclopirox#cite_note-pmid12760852-4, PubMed, PubMed, Wikipedia, http://europepmc.org/abstract/MED/20964457, http://europepmc.org/abstract/MED/23416050, http://en.wikipedia.org/wiki/Ciclopirox#cite_note-pmid12760852-4
#>   molecular_mechanism molecule_chembl_id parent_molecule_chembl_id record_id
#> 1                   1         CHEMBL1413                CHEMBL1413   1343970
#> 2                   1         CHEMBL1413                CHEMBL1413   1343970
#>   selectivity_comment site_id target_chembl_id variant_sequence
#> 1                  NA      NA    CHEMBL2363058               NA
#> 2                  NA      NA    CHEMBL2366381               NA
#> 
#> $page_meta
#> $page_meta$limit
#> [1] 20
#> 
#> $page_meta$`next`
#> NULL
#> 
#> $page_meta$offset
#> [1] 0
#> 
#> $page_meta$previous
#> NULL
#> 
#> $page_meta$total_count
#> [1] 2
```

The
[`getChemblResources()`](https://bhklab.github.io/AnnotationGx/reference/getChemblResources.md)
function returns a list of possible resources that can be queried:

``` r
getChemblResources() 
#>  [1] "activity"                  "assay"                    
#>  [3] "atc_class"                 "binding_site"             
#>  [5] "biotherapeutic"            "cell_line"                
#>  [7] "chembl_id_lookup"          "compound_record"          
#>  [9] "compound_structural_alert" "document"                 
#> [11] "document_similarity"       "document_term"            
#> [13] "drug"                      "drug_indication"          
#> [15] "drug_warning"              "go_slim"                  
#> [17] "image"                     "mechanism"                
#> [19] "metabolism"                "molecule"                 
#> [21] "molecule_form"             "organism"                 
#> [23] "protein_classification"    "similarity"               
#> [25] "source"                    "status"                   
#> [27] "substructure"              "target"                   
#> [29] "target_component"          "target_relation"          
#> [31] "tissue"                    "xref_source"
```

The `getChemblResourceFields(resource)` function returns a list of
possible fields that can be queried for a given resource:

``` r
getChemblResourceFields("mechanism")
#>  [1] "action_type"               "binding_site_comment"     
#>  [3] "direct_interaction"        "disease_efficacy"         
#>  [5] "max_phase"                 "mec_id"                   
#>  [7] "mechanism_comment"         "mechanism_of_action"      
#>  [9] "mechanism_refs"            "molecular_mechanism"      
#> [11] "molecule_chembl_id"        "parent_molecule_chembl_id"
#> [13] "record_id"                 "selectivity_comment"      
#> [15] "site_id"                   "target_chembl_id"         
#> [17] "variant_sequence"
```

The
[`getChemblFilterTypes()`](https://bhklab.github.io/AnnotationGx/reference/getChemblFilterTypes.md)
function returns a list of possible filter types.

``` r
getChemblFilterTypes()
#>  [1] "exact"       "iexact"      "contains"    "icontains"   "startswith" 
#>  [6] "istartswith" "endswith"    "iendswith"   "regex"       "iregex"     
#> [11] "gt"          "gte"         "lt"          "lte"         "range"      
#> [16] "in"          "isnull"      "search"      "only"
```
