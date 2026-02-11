# Get ChEMBL Mechanism

This function retrieves information about the mechanism of action for a
given ChEMBL ID.

## Usage

``` r
getChemblMechanism(
  chembl.ID,
  resources = "mechanism",
  field = "molecule_chembl_id",
  filter_type = "in",
  returnURL = FALSE,
  raw = FALSE
)
```

## Arguments

- chembl.ID:

  The ChEMBL ID of the molecule.

- resources:

  The ChEMBL resource to query (default: "mechanism").

- field:

  The field to filter on (default: "molecule_chembl_id").

- filter_type:

  The filter type to use (default: "in").

- returnURL:

  Logical indicating whether to return the constructed URL (default:
  FALSE).

- raw:

  Logical indicating whether to return the raw response JSON (default:
  FALSE).

## Value

A data.table containing the retrieved mechanism information.

## Examples

``` r
getChemblMechanism("CHEMBL1413")
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
getChemblMechanism("CHEMBL1413",
  resources = "mechanism", field = "molecule_chembl_id",
  filter_type = "in", returnURL = FALSE, raw = FALSE
)
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
