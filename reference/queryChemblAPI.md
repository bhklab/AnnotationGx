# Query the ChEMBL API

This function queries the ChEMBL API using the specified parameters and
returns the response in JSON format.

## Usage

``` r
queryChemblAPI(resource, field, filter_type, value, format = "json")
```

## Arguments

- resource:

  The resource to query in the ChEMBL API.

- field:

  The field to filter on in the ChEMBL API.

- filter_type:

  The type of filter to apply in the ChEMBL API.

- value:

  The value to filter on in the ChEMBL API.

- format:

  The format of the response (default is "json").

## Value

The response from the ChEMBL API in JSON format.

## Examples

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
#> 
#> 
```
