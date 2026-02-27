# Query UniChem for a compound.

This function queries the UniChem API for a compound based on the
provided parameters.

## Usage

``` r
queryUnichemCompound(
  compound,
  type,
  sourceID = NA_integer_,
  request_only = FALSE,
  raw = FALSE,
  progress = "Querying UniChem...",
  ...
)
```

## Arguments

- compound:

  `character`, `integer`, or a list of such values. When a vector or
  list is supplied, each element is queried and the results are returned
  as a named list.

- type:

  `character` The type of compound identifier to search for. Valid types
  are "uci", "inchi", "inchikey", and "sourceID".

- sourceID:

  `integer` The source ID to search for if the type is "sourceID". When
  querying multiple compounds, this can be a vector the same length as
  `compound` or a single value recycled to all queries. Defaults to
  `NA`.

- request_only:

  `boolean` Whether to return the request only. Defaults to FALSE.

- raw:

  `boolean` Whether to return the raw response. Defaults to FALSE.

- progress:

  `logical` or `character`. Passed through to
  `.perform_request_parallel()` when multiple compounds are supplied.
  Use a character string to customise the progress label. Defaults to
  `"Querying UniChem..."`.

- ...:

  Additional arguments.

## Value

For a single query, a list with the external mappings and the UniChem
mappings. For multiple queries, a named list of such results (one per
compound). If `raw = TRUE`, raw responses are returned instead.

## Examples

``` r
queryUnichemCompound(type = "sourceID", compound = "444795", sourceID = 22)
#> $External_Mappings
#>       compoundID             Name                            NameLong sourceID
#>           <char>           <char>                              <char>    <int>
#>   1:    CHEMBL38           chembl                              ChEMBL        1
#>   2:     DB00755         drugbank                            DrugBank        2
#>   3:         3KV         rcsb_pdb                            RCSB PDB        3
#>   4:         REA         rcsb_pdb                            RCSB PDB        3
#>   5: CHEBI:15367            chebi                               ChEBI        7
#>  ---                                                                          
#> 275:      260365           brenda                              Brenda       37
#> 276:        5126           brenda                              Brenda       37
#> 277:        6481           brenda                              Brenda       37
#> 278:    PD001430 probes_and_drugs                        Probes&Drugs       49
#> 279:      VITAAC             CCDC CSD (Cambridge Structural Database)       50
#>                                                                    sourceURL
#>                                                                       <char>
#>   1:                https://www.ebi.ac.uk/chembldb/compound/inspect/CHEMBL38
#>   2:                                   https://go.drugbank.com/drugs/DB00755
#>   3:                                         https://www.rcsb.org/ligand/3KV
#>   4:                                         https://www.rcsb.org/ligand/REA
#>   5:                                 https://www.ebi.ac.uk/chebi/CHEBI:15367
#>  ---                                                                        
#> 275:       https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=260365
#> 276:         https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=5126
#> 277:         https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=6481
#> 278:                         https://www.probes-drugs.org/compounds/PD001430
#> 279: https://www.ccdc.cam.ac.uk/structures/search?sid=UNICHEM&pid=csd:VITAAC
#> 
#> $UniChem_Mappings
#> $UniChem_Mappings$UniChem.UCI
#> [1] 538323
#> 
#> $UniChem_Mappings$UniChem.InchiKey
#> [1] "SHGAZHPCJJPHSC-YCNIQYBTSA-N"
#> 
#> $UniChem_Mappings$UniChem.Inchi
#> [1] "InChI=1S/C20H28O2/c1-15(8-6-9-16(2)14-19(21)22)11-12-18-17(3)10-7-13-20(18,4)5/h6,8-9,11-12,14H,7,10,13H2,1-5H3,(H,21,22)/b9-6+,12-11+,15-8+,16-14+"
#> 
#> $UniChem_Mappings$UniChem.formula
#> [1] "C20H28O2"
#> 
#> $UniChem_Mappings$UniChem.connections
#> [1] "1-15(8-6-9-16(2)14-19(21)22)11-12-18-17(3)10-7-13-20(18,4)5"
#> 
#> $UniChem_Mappings$UniChem.hAtoms
#> [1] "6,8-9,11-12,14H,7,10,13H2,1-5H3,(H,21,22)"
#> 
#> 
```
