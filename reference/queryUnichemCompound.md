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
#>              compoundID             Name
#>                  <char>           <char>
#>  1:            CHEMBL38           chembl
#>  2:             DB00755         drugbank
#>  3:                 3KV         rcsb_pdb
#>  4:                 REA         rcsb_pdb
#>  5:         CHEBI:15367            chebi
#>  6:          5688UTC01R           fdasrs
#>  7:                3145       surechembl
#>  8:         HMDB0001852             hmdb
#>  9:              444795          pubchem
#> 10: Molport-000-883-857          molport
#> 11:               31883        bindingdb
#> 12:        LMPR01090019        lipidmaps
#> 13:                2722      drugcentral
#> 14:               12679           brenda
#> 15:              193381           brenda
#> 16:               21231           brenda
#> 17:                2354           brenda
#> 18:              260365           brenda
#> 19:                5126           brenda
#> 20:                6481           brenda
#> 21:            PD001430 probes_and_drugs
#> 22:              VITAAC             CCDC
#>              compoundID             Name
#>                  <char>           <char>
#>                                        NameLong sourceID
#>                                          <char>    <int>
#>  1:                                      ChEMBL        1
#>  2:                                    DrugBank        2
#>  3:                                    RCSB PDB        3
#>  4:                                    RCSB PDB        3
#>  5:                                       ChEBI        7
#>  6: FDA/USP Substance Registration System (SRS)       14
#>  7:                                  SureChEMBL       15
#>  8:                                        HMDB       18
#>  9:                           PubChem Compounds       22
#> 10:                                     MolPort       28
#> 11:                                   BindingDB       31
#> 12:       LIPID MAPS® Structure Database (LMSD)       33
#> 13:                                 DrugCentral       34
#> 14:                                      Brenda       37
#> 15:                                      Brenda       37
#> 16:                                      Brenda       37
#> 17:                                      Brenda       37
#> 18:                                      Brenda       37
#> 19:                                      Brenda       37
#> 20:                                      Brenda       37
#> 21:                                Probes&Drugs       49
#> 22:         CSD (Cambridge Structural Database)       50
#>                                        NameLong sourceID
#>                                          <char>    <int>
#>                                                                             sourceURL
#>                                                                                <char>
#>  1:                          https://www.ebi.ac.uk/chembldb/compound/inspect/CHEMBL38
#>  2:                                             https://go.drugbank.com/drugs/DB00755
#>  3:                                                   https://www.rcsb.org/ligand/3KV
#>  4:                                                   https://www.rcsb.org/ligand/REA
#>  5:                                           https://www.ebi.ac.uk/chebi/CHEBI:15367
#>  6:              https://d20b1koi85gdl2.cloudfront.net/uniisearch/srs/unii/5688UTC01R
#>  7:                                          https://www.surechembl.org/chemical/3145
#>  8:                                       https://www.hmdb.ca/metabolites/HMDB0001852
#>  9:                                  https://pubchem.ncbi.nlm.nih.gov/compound/444795
#> 10:                         https://www.molport.com/shop/compound/Molport-000-883-857
#> 11: https://www.bindingdb.org/bind/chemsearch/marvin/MolStructure.jsp?monomerid=31883
#> 12:                   https://www.lipidmaps.org/data/LMSDRecord.php?LMID=LMPR01090019
#> 13:                                             https://drugcentral.org/drugcard/2722
#> 14:                  https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=12679
#> 15:                 https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=193381
#> 16:                  https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=21231
#> 17:                   https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=2354
#> 18:                 https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=260365
#> 19:                   https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=5126
#> 20:                   https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=6481
#> 21:                                   https://www.probes-drugs.org/compounds/PD001430
#> 22:           https://www.ccdc.cam.ac.uk/structures/search?sid=UNICHEM&pid=csd:VITAAC
#>                                                                             sourceURL
#>                                                                                <char>
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
