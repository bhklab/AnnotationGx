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
#>                  compoundID             Name
#>                      <char>           <char>
#>  1:                CHEMBL38           chembl
#>  2:                 DB00755         drugbank
#>  3:                     REA              pdb
#>  4:                    2644           gtopdb
#>  5:                12014646     pubchem_dotf
#>  6:                  C00777      kegg_ligand
#>  7:                   15367            chebi
#>  8:            SAM002264647          nih_ncc
#>  9:        ZINC000012358651             zinc
#> 10:                 1934590       emolecules
#> 11:               tretinoin            atlas
#> 12:           retinoic acid            atlas
#> 13: all-trans retinoic acid            atlas
#> 14:     9-cis retinoic-acid            atlas
#> 15:              5688UTC01R           fdasrs
#> 16:         SCHEMBL19091395       surechembl
#> 17:             SCHEMBL3145       surechembl
#> 18:             PA164746900         pharmgkb
#> 19:             HMDB0001852             hmdb
#> 20:      Tretinoin(Aberela)          selleck
#> 21:                14849563  pubchem_tpharma
#> 22:                14825303  pubchem_tpharma
#> 23:                  444795          pubchem
#> 24:                60021090      nmrshiftdb2
#> 25:               LSM-42854            lincs
#> 26:               4759-48-2            actor
#> 27:                302-79-4            actor
#> 28:             187175-63-9            actor
#> 29:             J2.378.058E          nikkaji
#> 30:             J1.313.469C          nikkaji
#> 31:               J623.910B          nikkaji
#> 32:               J970.183D          nikkaji
#> 33:                 J1.518K          nikkaji
#> 34:               J494.243D          nikkaji
#> 35:               J690.379G          nikkaji
#> 36:               J646.157C          nikkaji
#> 37:               J646.158A          nikkaji
#> 38:               J528.606I          nikkaji
#> 39:                   31883        bindingdb
#> 40:                  323588        bindingdb
#> 41:           DTXSID7021239          comptox
#> 42:            LMPR01090019        lipidmaps
#> 43:                    2722      drugcentral
#> 44:              MTBLC15367     metabolights
#> 45:                    6481           brenda
#> 46:                   12679           brenda
#> 47:                   21231           brenda
#> 48:                    2354           brenda
#> 49:                    5126           brenda
#> 50:                  193381           brenda
#> 51:                  260365           brenda
#> 52:               CB6222631     chemicalbook
#> 53:               TRETINOIN         dailymed
#> 54:                  ABEREL   clinicaltrials
#> 55:                  EUDYNA   clinicaltrials
#> 56:                VESANOID   clinicaltrials
#> 57:                 ALTRENO   clinicaltrials
#> 58:               TRETINOIN   clinicaltrials
#> 59:                 RETIN A   clinicaltrials
#> 60:                 RETIN-A   clinicaltrials
#> 61:           RETINOIC ACID   clinicaltrials
#> 62:                  RENOVA   clinicaltrials
#> 63:                   AVITA   clinicaltrials
#> 64:              NSC-122758   clinicaltrials
#> 65:   TRETINOIN MICROSPHERE           rxnorm
#> 66:                 ATRALIN           rxnorm
#> 67:                 RETIN-A           rxnorm
#> 68:                 ALTRENO           rxnorm
#> 69:                 ACTICIN           rxnorm
#> 70:                   AVITA           rxnorm
#> 71:                  RENOVA           rxnorm
#> 72:               TRETINOIN           rxnorm
#> 73:                HY-14649   MedChemExpress
#> 74:                PD001430 probes_and_drugs
#> 75:                  VITAAC             CCDC
#>                  compoundID             Name
#>                      <char>           <char>
#>                                                    NameLong sourceID
#>                                                      <char>    <int>
#>  1:                                                  ChEMBL        1
#>  2:                                                DrugBank        2
#>  3:                         PDBe (Protein Data Bank Europe)        3
#>  4:                                   Guide to Pharmacology        4
#>  5:                  PubChem ('Drugs of the Future' subset)        5
#>  6:   KEGG (Kyoto Encyclopedia of Genes and Genomes) Ligand        6
#>  7:       ChEBI (Chemical Entities of Biological Interest).        7
#>  8:                                 NIH Clinical Collection        8
#>  9:                                                    ZINC        9
#> 10:                                              eMolecules       10
#> 11:                                   Gene Expression Atlas       12
#> 12:                                   Gene Expression Atlas       12
#> 13:                                   Gene Expression Atlas       12
#> 14:                                   Gene Expression Atlas       12
#> 15:             FDA/USP Substance Registration System (SRS)       14
#> 16:                                              SureChEMBL       15
#> 17:                                              SureChEMBL       15
#> 18:                                                PharmGKB       17
#> 19:                        Human Metabolome Database (HMDB)       18
#> 20:                                                 Selleck       20
#> 21:                       PubChem ('Thomson Pharma' subset)       21
#> 22:                       PubChem ('Thomson Pharma' subset)       21
#> 23:                                       PubChem Compounds       22
#> 24:                                              NMRShiftDB       24
#> 25: Library of Integrated Network-based Cellular Signatures       25
#> 26:                                                   ACToR       26
#> 27:                                                   ACToR       26
#> 28:                                                   ACToR       26
#> 29:                                                 Nikkaji       29
#> 30:                                                 Nikkaji       29
#> 31:                                                 Nikkaji       29
#> 32:                                                 Nikkaji       29
#> 33:                                                 Nikkaji       29
#> 34:                                                 Nikkaji       29
#> 35:                                                 Nikkaji       29
#> 36:                                                 Nikkaji       29
#> 37:                                                 Nikkaji       29
#> 38:                                                 Nikkaji       29
#> 39:                                               BindingDB       31
#> 40:                                               BindingDB       31
#> 41: EPA (Environmental Protection Agency) CompTox Dashboard       32
#> 42:                                               LipidMaps       33
#> 43:                                             DrugCentral       34
#> 44:                                            Metabolights       36
#> 45:                                                  Brenda       37
#> 46:                                                  Brenda       37
#> 47:                                                  Brenda       37
#> 48:                                                  Brenda       37
#> 49:                                                  Brenda       37
#> 50:                                                  Brenda       37
#> 51:                                                  Brenda       37
#> 52:                                            ChemicalBook       39
#> 53:                                                DailyMed       45
#> 54:                                          clinicaltrials       46
#> 55:                                          clinicaltrials       46
#> 56:                                          clinicaltrials       46
#> 57:                                          clinicaltrials       46
#> 58:                                          clinicaltrials       46
#> 59:                                          clinicaltrials       46
#> 60:                                          clinicaltrials       46
#> 61:                                          clinicaltrials       46
#> 62:                                          clinicaltrials       46
#> 63:                                          clinicaltrials       46
#> 64:                                          clinicaltrials       46
#> 65:                                                  rxnorm       47
#> 66:                                                  rxnorm       47
#> 67:                                                  rxnorm       47
#> 68:                                                  rxnorm       47
#> 69:                                                  rxnorm       47
#> 70:                                                  rxnorm       47
#> 71:                                                  rxnorm       47
#> 72:                                                  rxnorm       47
#> 73:                                          MedChemExpress       48
#> 74:                                        Probes And Drugs       49
#> 75:                                                    CCDC       50
#>                                                    NameLong sourceID
#>                                                      <char>    <int>
#>                                                                                                                           sourceURL
#>                                                                                                                              <char>
#>  1:                                                                        https://www.ebi.ac.uk/chembldb/compound/inspect/CHEMBL38
#>  2:                                                                                            http://www.drugbank.ca/drugs/DB00755
#>  3:                                                                http://www.ebi.ac.uk/pdbe-srv/pdbechem/chemicalCompound/show/REA
#>  4:                                                      http://www.guidetopharmacology.org/GRAC/LigandDisplayForward?ligandId=2644
#>  5:                                                                              http://pubchem.ncbi.nlm.nih.gov/substance/12014646
#>  6:                                                                                  http://www.genome.jp/dbget-bin/www_bget?C00777
#>  7:                                                                    http://www.ebi.ac.uk/chebi/searchId.do?chebiId=CHEBI%3A15367
#>  8:                                                                                                                    SAM002264647
#>  9:                                                                           http://zinc15.docking.org/substances/ZINC000012358651
#> 10:                                                                             https://www.emolecules.com/cgi-bin/more?vid=1934590
#> 11:                                                                         http://www.ebi.ac.uk/gxa/query?conditionQuery=tretinoin
#> 12:                                                                     http://www.ebi.ac.uk/gxa/query?conditionQuery=retinoic acid
#> 13:                                                           http://www.ebi.ac.uk/gxa/query?conditionQuery=all-trans retinoic acid
#> 14:                                                               http://www.ebi.ac.uk/gxa/query?conditionQuery=9-cis retinoic-acid
#> 15:                                                                        https://precision.fda.gov/uniisearch/srs/unii/5688UTC01R
#> 16:                                                                             https://www.surechembl.org/chemical/SCHEMBL19091395
#> 17:                                                                                 https://www.surechembl.org/chemical/SCHEMBL3145
#> 18:                                                                                       https://www.pharmgkb.org/drug/PA164746900
#> 19:                                                                                      http://www.hmdb.ca/metabolites/HMDB0001852
#> 20:                                                                     http://www.selleckchem.com/products/Tretinoin(Aberela).html
#> 21:                                                                              http://pubchem.ncbi.nlm.nih.gov/substance/14849563
#> 22:                                                                              http://pubchem.ncbi.nlm.nih.gov/substance/14825303
#> 23:                                                                                 http://pubchem.ncbi.nlm.nih.gov/compound/444795
#> 24:                                                                                         http://nmrshiftdb.org/molecule/60021090
#> 25:                                                                            http://identifiers.org/lincs.smallmolecule/LSM-42854
#> 26:                                                                       http://actor.epa.gov/actor/chemical.xhtml?casrn=4759-48-2
#> 27:                                                                        http://actor.epa.gov/actor/chemical.xhtml?casrn=302-79-4
#> 28:                                                                     http://actor.epa.gov/actor/chemical.xhtml?casrn=187175-63-9
#> 29:                                                                     http://jglobal.jst.go.jp/en/redirect?Nikkaji_No=J2.378.058E
#> 30:                                                                     http://jglobal.jst.go.jp/en/redirect?Nikkaji_No=J1.313.469C
#> 31:                                                                       http://jglobal.jst.go.jp/en/redirect?Nikkaji_No=J623.910B
#> 32:                                                                       http://jglobal.jst.go.jp/en/redirect?Nikkaji_No=J970.183D
#> 33:                                                                         http://jglobal.jst.go.jp/en/redirect?Nikkaji_No=J1.518K
#> 34:                                                                       http://jglobal.jst.go.jp/en/redirect?Nikkaji_No=J494.243D
#> 35:                                                                       http://jglobal.jst.go.jp/en/redirect?Nikkaji_No=J690.379G
#> 36:                                                                       http://jglobal.jst.go.jp/en/redirect?Nikkaji_No=J646.157C
#> 37:                                                                       http://jglobal.jst.go.jp/en/redirect?Nikkaji_No=J646.158A
#> 38:                                                                       http://jglobal.jst.go.jp/en/redirect?Nikkaji_No=J528.606I
#> 39:                                                http://www.bindingdb.org/bind/chemsearch/marvin/MolStructure.jsp?monomerid=31883
#> 40:                                               http://www.bindingdb.org/bind/chemsearch/marvin/MolStructure.jsp?monomerid=323588
#> 41:                                                                                 https://comptox.epa.gov/dashboard/DTXSID7021239
#> 42:                                                                  http://www.lipidmaps.org/data/LMSDRecord.php?LMID=LMPR01090019
#> 43:                                                                                            http://drugcentral.org/drugcard/2722
#> 44:                                                                                    http://www.ebi.ac.uk/metabolights/MTBLC15367
#> 45:                                                                 https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=6481
#> 46:                                                                https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=12679
#> 47:                                                                https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=21231
#> 48:                                                                 https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=2354
#> 49:                                                                 https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=5126
#> 50:                                                               https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=193381
#> 51:                                                               https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=260365
#> 52:                                                               https://www.chemicalbook.com/ChemicalProductProperty_EN_CB6222631
#> 53: https://dailymed.nlm.nih.gov/dailymed/search.cfm?adv=1&labeltype=human&query=ACTIVEMOIETY:(TRETINOIN)+OR+INGREDIENT:(TRETINOIN)
#> 54:                                                                    https://www.clinicaltrials.gov/ct2/results?cond=&term=ABEREL
#> 55:                                                                    https://www.clinicaltrials.gov/ct2/results?cond=&term=EUDYNA
#> 56:                                                                  https://www.clinicaltrials.gov/ct2/results?cond=&term=VESANOID
#> 57:                                                                   https://www.clinicaltrials.gov/ct2/results?cond=&term=ALTRENO
#> 58:                                                                 https://www.clinicaltrials.gov/ct2/results?cond=&term=TRETINOIN
#> 59:                                                                   https://www.clinicaltrials.gov/ct2/results?cond=&term=RETIN A
#> 60:                                                                   https://www.clinicaltrials.gov/ct2/results?cond=&term=RETIN-A
#> 61:                                                             https://www.clinicaltrials.gov/ct2/results?cond=&term=RETINOIC ACID
#> 62:                                                                    https://www.clinicaltrials.gov/ct2/results?cond=&term=RENOVA
#> 63:                                                                     https://www.clinicaltrials.gov/ct2/results?cond=&term=AVITA
#> 64:                                                                https://www.clinicaltrials.gov/ct2/results?cond=&term=NSC-122758
#> 65:                                                           https://mor.nlm.nih.gov/RxNav/search?searchBy=RXCUI&searchTerm=221175
#> 66:                                                           https://mor.nlm.nih.gov/RxNav/search?searchBy=RXCUI&searchTerm=728470
#> 67:                                                           https://mor.nlm.nih.gov/RxNav/search?searchBy=RXCUI&searchTerm=153101
#> 68:                                                          https://mor.nlm.nih.gov/RxNav/search?searchBy=RXCUI&searchTerm=2055004
#> 69:                                                           https://mor.nlm.nih.gov/RxNav/search?searchBy=RXCUI&searchTerm=214998
#> 70:                                                           https://mor.nlm.nih.gov/RxNav/search?searchBy=RXCUI&searchTerm=215492
#> 71:                                                           https://mor.nlm.nih.gov/RxNav/search?searchBy=RXCUI&searchTerm=846950
#> 72:                                                            https://mor.nlm.nih.gov/RxNav/search?searchBy=RXCUI&searchTerm=10753
#> 73:                                                                                         https://www.medchemexpress.com/HY-14649
#> 74:                                                                                  https://www.probes-drugs.org/compound/PD001430
#> 75:                                                         https://www.ccdc.cam.ac.uk/structures/search?sid=UNICHEM&pid=csd:VITAAC
#>                                                                                                                           sourceURL
#>                                                                                                                              <char>
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
