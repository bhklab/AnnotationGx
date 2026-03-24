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
#>  5:                 REA             pdbe
#>  6:         CHEBI:15367            chebi
#>  7:          5688UTC01R           fdasrs
#>  8:                3145       surechembl
#>  9:         HMDB0001852             hmdb
#> 10:              444795          pubchem
#> 11: Molport-000-883-857          molport
#> 12:               31883        bindingdb
#> 13:        LMPR01090019        lipidmaps
#> 14:                2722      drugcentral
#> 15:               12679           brenda
#> 16:              193381           brenda
#> 17:               21231           brenda
#> 18:                2354           brenda
#> 19:              260365           brenda
#> 20:                5126           brenda
#> 21:                6481           brenda
#> 22:            PD001430 probes_and_drugs
#> 23:              VITAAC             CCDC
#>              compoundID             Name
#>                  <char>           <char>
#>                                        NameLong sourceID
#>                                          <char>    <int>
#>  1:                                      ChEMBL        1
#>  2:                                    DrugBank        2
#>  3:                                    RCSB PDB        3
#>  4:                                    RCSB PDB        3
#>  5:                 Protein Data Bank in Europe        5
#>  6:                                       ChEBI        7
#>  7: FDA/USP Substance Registration System (SRS)       14
#>  8:                                  SureChEMBL       15
#>  9:                                        HMDB       18
#> 10:                           PubChem Compounds       22
#> 11:                                     MolPort       28
#> 12:                                   BindingDB       31
#> 13:       LIPID MAPS® Structure Database (LMSD)       33
#> 14:                                 DrugCentral       34
#> 15:                                      Brenda       37
#> 16:                                      Brenda       37
#> 17:                                      Brenda       37
#> 18:                                      Brenda       37
#> 19:                                      Brenda       37
#> 20:                                      Brenda       37
#> 21:                                      Brenda       37
#> 22:                                Probes&Drugs       49
#> 23:         CSD (Cambridge Structural Database)       50
#>                                        NameLong sourceID
#>                                          <char>    <int>
#>                                                                             sourceURL
#>                                                                                <char>
#>  1:                          https://www.ebi.ac.uk/chembldb/compound/inspect/CHEMBL38
#>  2:                                             https://go.drugbank.com/drugs/DB00755
#>  3:                                                   https://www.rcsb.org/ligand/3KV
#>  4:                                                   https://www.rcsb.org/ligand/REA
#>  5:                 https://www.ebi.ac.uk/pdbe-srv/pdbechem/chemicalCompound/show/REA
#>  6:                                           https://www.ebi.ac.uk/chebi/CHEBI:15367
#>  7:              https://d20b1koi85gdl2.cloudfront.net/uniisearch/srs/unii/5688UTC01R
#>  8:                                          https://www.surechembl.org/chemical/3145
#>  9:                                       https://www.hmdb.ca/metabolites/HMDB0001852
#> 10:                                  https://pubchem.ncbi.nlm.nih.gov/compound/444795
#> 11:                         https://www.molport.com/shop/compound/Molport-000-883-857
#> 12: https://www.bindingdb.org/bind/chemsearch/marvin/MolStructure.jsp?monomerid=31883
#> 13:                   https://www.lipidmaps.org/data/LMSDRecord.php?LMID=LMPR01090019
#> 14:                                             https://drugcentral.org/drugcard/2722
#> 15:                  https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=12679
#> 16:                 https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=193381
#> 17:                  https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=21231
#> 18:                   https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=2354
#> 19:                 https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=260365
#> 20:                   https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=5126
#> 21:                   https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=6481
#> 22:                                   https://www.probes-drugs.org/compounds/PD001430
#> 23:           https://www.ccdc.cam.ac.uk/structures/search?sid=UNICHEM&pid=csd:VITAAC
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
