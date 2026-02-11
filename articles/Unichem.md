# Querying Unichem Database

## Introduction to the Unichem API

The UniChem database provides a publicly available REST API for
programmatic retrieval of mappings from standardized structural compound
identifiers to unique compound IDs across a range of large online
cheminformatic databases such as PubChem, ChEMBL, DrugBank and many
more. The service accepts POST requests to two different end-points:
`/compound` and `/connectivity`. Both endpoints accept query parameters
via the POST body in JSON format. The `/compound` API returns exact
matches for the queried compound, while the `/connectivity` API uses
layers of the International Chemical Identifier (InChI) of the query
compound to return exact matches as well as structurally related
compounds such as isomers, salts, ionizations and more. \[@UniChemBeta;
@chambersUniChemUnifiedChemical2013\]

The functions in `AnnotationGx` have been designed to allow package
users to easily query UniChem resources without any pre-existing
knowledge of HTTP requests or the API specifications. In doing so we
hope to provide an R native interface for mapping between various
cheminformatic databases, accessible to anyone familar with using R
functions!

``` r
library(AnnotationGx)
```

## Available Databases

To see a table of database identifiers available via UniChem, you can
call the `getUniChemSources` function. By default, just the database
shortname (“Name”) and UniChem’s ID for it (“SourceID”) columns are
returned. To return all columns, pass the `all_columns = TRUE` argument

``` r
getUnichemSources()
#>                 Name SourceID
#>               <char>    <int>
#>  1:           chembl        1
#>  2:         drugbank        2
#>  3:              pdb        3
#>  4:           gtopdb        4
#>  5:     pubchem_dotf        5
#>  6:      kegg_ligand        6
#>  7:            chebi        7
#>  8:          nih_ncc        8
#>  9:             zinc        9
#> 10:       emolecules       10
#> 11:            atlas       12
#> 12:           fdasrs       14
#> 13:       surechembl       15
#> 14:         pharmgkb       17
#> 15:             hmdb       18
#> 16:          selleck       20
#> 17:  pubchem_tpharma       21
#> 18:          pubchem       22
#> 19:            mcule       23
#> 20:      nmrshiftdb2       24
#> 21:            lincs       25
#> 22:            actor       26
#> 23:            recon       27
#> 24:          molport       28
#> 25:          nikkaji       29
#> 26:        bindingdb       31
#> 27:          comptox       32
#> 28:        lipidmaps       33
#> 29:      drugcentral       34
#> 30:     carotenoiddb       35
#> 31:     metabolights       36
#> 32:           brenda       37
#> 33:             rhea       38
#> 34:     chemicalbook       39
#> 35:      swisslipids       41
#> 36:         dailymed       45
#> 37:   clinicaltrials       46
#> 38:           rxnorm       47
#> 39:   MedChemExpress       48
#> 40: probes_and_drugs       49
#> 41:             CCDC       50
#>                 Name SourceID
#>               <char>    <int>
```

When mapping using the `queryUnichemCompound` function, these are the
sources that can be used from, and the databases to which the compound
mappings will be returned.

## Querying UniChem Compound API

The `queryUnichemCompound` function allows you to query the UniChem
Compound API to retrieve mappings for a given compound identifier. The
function takes two mandatory arguments. The first is the `compound`
argument which is the compound identifier to be queried. The second is
the `type` argument which is the type of compound identifier to search
for. Options are “uci”, “inchi”, “inchikey”, and “sourceID”. The
`sourceID` argument is optional and is only required if the `type`
argument is “sourceID”.

The function returns a list of:

1.  “External_Mappings” `data.table` containing the mapping to other
    Databases with the following headings:
    1.  “compoundID” `character` The compound identifier
    2.  “Name” `character` The name of the database
    3.  “NameLong” `character` The long name of the database
    4.  “SourceID” `character` The UniChem Source ID
    5.  “sourceURL” `character` The URL of the source
2.  “UniChem_Mappings” `list` of the following six mappings:
    1.  “UCI” `character` The UniChem Identifier
    2.  “InchiKey” `character` The InChIKey
    3.  “Inchi” `character` The InChI
    4.  “formula” `character` The molecular formula
    5.  “connections” `character` connection representation
        “1-6(10)13-8-5-3-2-4-7(8)9(11)12”
    6.  “hAtoms” `character` hydrogen atom connections
        “2-5H,1H3,(H,11,12)”

##### Example Searching using `uci` (UniChem Identifier)

Note: This type of query requires you to know the UniChem Identifier for
the compound.

``` r

queryUnichemCompound(compound = "161671", type = "uci")
#> $External_Mappings
#>                       compoundID             Name
#>                           <char>           <char>
#>  1:                     CHEMBL25           chembl
#>  2:                      DB00945         drugbank
#>  3:                          AIN              pdb
#>  4:                         4139           gtopdb
#>  5:                     24714725     pubchem_dotf
#>  6:                       C01405      kegg_ligand
#>  7:                        15365            chebi
#>  8:             ZINC000000000053             zinc
#>  9:                       474821       emolecules
#> 10:         acetylsalicylic acid            atlas
#> 11:                      aspirin            atlas
#> 12:                   R16CO5Y76E           fdasrs
#> 13:                  SCHEMBL1353       surechembl
#> 14:                     PA448497         pharmgkb
#> 15:                  HMDB0001879             hmdb
#> 16: aspirin-acetylsalicylic-acid          selleck
#> 17:                     15195166  pubchem_tpharma
#> 18:                         2244          pubchem
#> 19:             MCULE-3199019536            mcule
#> 20:                     20038075      nmrshiftdb2
#> 21:                     LSM-5288            lincs
#> 22:                   11126-35-5            actor
#> 23:                      50-78-2            actor
#> 24:                      J2.300K          nikkaji
#> 25:                        22360        bindingdb
#> 26:                DTXSID5020108          comptox
#> 27:                           74      drugcentral
#> 28:                         6476           brenda
#> 29:                        32748           brenda
#> 30:                         4779           brenda
#> 31:                         3100           brenda
#> 32:                       159662           brenda
#> 33:                         2261           brenda
#> 34:                    CB4421683     chemicalbook
#> 35:                    CB5114818     chemicalbook
#> 36:                      ASPIRIN         dailymed
#> 37:         ACETYLSALICYLIC ACID   clinicaltrials
#> 38:                      DURLAZA   clinicaltrials
#> 39:                      ASPIRIN   clinicaltrials
#> 40:            ASPIRIN DL-LYSINE   clinicaltrials
#> 41:                    VENOPIRIN   clinicaltrials
#> 42:                     MEASURIN   clinicaltrials
#> 43:                   BAY1019036   clinicaltrials
#> 44:      LYSINE ACETYLSALICYLATE   clinicaltrials
#> 45:                      ECOTRIN           rxnorm
#> 46:                      ASPIRIN           rxnorm
#> 47:            ACETYL SALICYLATE           rxnorm
#> 48:                      DURLAZA           rxnorm
#> 49:                     HY-14654   MedChemExpress
#> 50:                     PD002467 probes_and_drugs
#> 51:                       ACSALA             CCDC
#>                       compoundID             Name
#>                           <char>           <char>
#>                                                    NameLong sourceID
#>                                                      <char>    <int>
#>  1:                                                  ChEMBL        1
#>  2:                                                DrugBank        2
#>  3:                         PDBe (Protein Data Bank Europe)        3
#>  4:                                   Guide to Pharmacology        4
#>  5:                  PubChem ('Drugs of the Future' subset)        5
#>  6:   KEGG (Kyoto Encyclopedia of Genes and Genomes) Ligand        6
#>  7:       ChEBI (Chemical Entities of Biological Interest).        7
#>  8:                                                    ZINC        9
#>  9:                                              eMolecules       10
#> 10:                                   Gene Expression Atlas       12
#> 11:                                   Gene Expression Atlas       12
#> 12:             FDA/USP Substance Registration System (SRS)       14
#> 13:                                              SureChEMBL       15
#> 14:                                                PharmGKB       17
#> 15:                        Human Metabolome Database (HMDB)       18
#> 16:                                                 Selleck       20
#> 17:                       PubChem ('Thomson Pharma' subset)       21
#> 18:                                       PubChem Compounds       22
#> 19:                                                   Mcule       23
#> 20:                                              NMRShiftDB       24
#> 21: Library of Integrated Network-based Cellular Signatures       25
#> 22:                                                   ACToR       26
#> 23:                                                   ACToR       26
#> 24:                                                 Nikkaji       29
#> 25:                                               BindingDB       31
#> 26: EPA (Environmental Protection Agency) CompTox Dashboard       32
#> 27:                                             DrugCentral       34
#> 28:                                                  Brenda       37
#> 29:                                                  Brenda       37
#> 30:                                                  Brenda       37
#> 31:                                                  Brenda       37
#> 32:                                                  Brenda       37
#> 33:                                                  Brenda       37
#> 34:                                            ChemicalBook       39
#> 35:                                            ChemicalBook       39
#> 36:                                                DailyMed       45
#> 37:                                          clinicaltrials       46
#> 38:                                          clinicaltrials       46
#> 39:                                          clinicaltrials       46
#> 40:                                          clinicaltrials       46
#> 41:                                          clinicaltrials       46
#> 42:                                          clinicaltrials       46
#> 43:                                          clinicaltrials       46
#> 44:                                          clinicaltrials       46
#> 45:                                                  rxnorm       47
#> 46:                                                  rxnorm       47
#> 47:                                                  rxnorm       47
#> 48:                                                  rxnorm       47
#> 49:                                          MedChemExpress       48
#> 50:                                        Probes And Drugs       49
#> 51:                                                    CCDC       50
#>                                                    NameLong sourceID
#>                                                      <char>    <int>
#>                                                                                                                                                                                          sourceURL
#>                                                                                                                                                                                             <char>
#>  1:                                                                                                                                       https://www.ebi.ac.uk/chembldb/compound/inspect/CHEMBL25
#>  2:                                                                                                                                                           http://www.drugbank.ca/drugs/DB00945
#>  3:                                                                                                                               http://www.ebi.ac.uk/pdbe-srv/pdbechem/chemicalCompound/show/AIN
#>  4:                                                                                                                     http://www.guidetopharmacology.org/GRAC/LigandDisplayForward?ligandId=4139
#>  5:                                                                                                                                             http://pubchem.ncbi.nlm.nih.gov/substance/24714725
#>  6:                                                                                                                                                 http://www.genome.jp/dbget-bin/www_bget?C01405
#>  7:                                                                                                                                   http://www.ebi.ac.uk/chebi/searchId.do?chebiId=CHEBI%3A15365
#>  8:                                                                                                                                          http://zinc15.docking.org/substances/ZINC000000000053
#>  9:                                                                                                                                             https://www.emolecules.com/cgi-bin/more?vid=474821
#> 10:                                                                                                                             http://www.ebi.ac.uk/gxa/query?conditionQuery=acetylsalicylic acid
#> 11:                                                                                                                                          http://www.ebi.ac.uk/gxa/query?conditionQuery=aspirin
#> 12:                                                                                                                                       https://precision.fda.gov/uniisearch/srs/unii/R16CO5Y76E
#> 13:                                                                                                                                                https://www.surechembl.org/chemical/SCHEMBL1353
#> 14:                                                                                                                                                         https://www.pharmgkb.org/drug/PA448497
#> 15:                                                                                                                                                     http://www.hmdb.ca/metabolites/HMDB0001879
#> 16:                                                                                                                          http://www.selleckchem.com/products/aspirin-acetylsalicylic-acid.html
#> 17:                                                                                                                                             http://pubchem.ncbi.nlm.nih.gov/substance/15195166
#> 18:                                                                                                                                                  http://pubchem.ncbi.nlm.nih.gov/compound/2244
#> 19:                                                                                                                                                             https://mcule.com/MCULE-3199019536
#> 20:                                                                                                                                                        http://nmrshiftdb.org/molecule/20038075
#> 21:                                                                                                                                            http://identifiers.org/lincs.smallmolecule/LSM-5288
#> 22:                                                                                                                                     http://actor.epa.gov/actor/chemical.xhtml?casrn=11126-35-5
#> 23:                                                                                                                                        http://actor.epa.gov/actor/chemical.xhtml?casrn=50-78-2
#> 24:                                                                                                                                        http://jglobal.jst.go.jp/en/redirect?Nikkaji_No=J2.300K
#> 25:                                                                                                               http://www.bindingdb.org/bind/chemsearch/marvin/MolStructure.jsp?monomerid=22360
#> 26:                                                                                                                                                https://comptox.epa.gov/dashboard/DTXSID5020108
#> 27:                                                                                                                                                             http://drugcentral.org/drugcard/74
#> 28:                                                                                                                                https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=6476
#> 29:                                                                                                                               https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=32748
#> 30:                                                                                                                                https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=4779
#> 31:                                                                                                                                https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=3100
#> 32:                                                                                                                              https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=159662
#> 33:                                                                                                                                https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=2261
#> 34:                                                                                                                              https://www.chemicalbook.com/ChemicalProductProperty_EN_CB4421683
#> 35:                                                                                                                              https://www.chemicalbook.com/ChemicalProductProperty_EN_CB5114818
#> 36:                                                                    https://dailymed.nlm.nih.gov/dailymed/search.cfm?adv=1&labeltype=human&query=ACTIVEMOIETY:(ASPIRIN)+OR+INGREDIENT:(ASPIRIN)
#> 37:                                                                                                                     https://www.clinicaltrials.gov/ct2/results?cond=&term=ACETYLSALICYLIC ACID
#> 38:                                                                                                                                  https://www.clinicaltrials.gov/ct2/results?cond=&term=DURLAZA
#> 39:                                                                                                                                  https://www.clinicaltrials.gov/ct2/results?cond=&term=ASPIRIN
#> 40:                                                                                                                        https://www.clinicaltrials.gov/ct2/results?cond=&term=ASPIRIN DL-LYSINE
#> 41:                                                                                                                                https://www.clinicaltrials.gov/ct2/results?cond=&term=VENOPIRIN
#> 42:                                                                                                                                 https://www.clinicaltrials.gov/ct2/results?cond=&term=MEASURIN
#> 43:                                                                                                                               https://www.clinicaltrials.gov/ct2/results?cond=&term=BAY1019036
#> 44:                                                                                                                  https://www.clinicaltrials.gov/ct2/results?cond=&term=LYSINE ACETYLSALICYLATE
#> 45:                                                                                                                          https://mor.nlm.nih.gov/RxNav/search?searchBy=RXCUI&searchTerm=202554
#> 46:                                                                                                                            https://mor.nlm.nih.gov/RxNav/search?searchBy=RXCUI&searchTerm=1191
#> 47:                                                                                                                           https://mor.nlm.nih.gov/RxNav/search?searchBy=RXCUI&searchTerm=91101
#> 48:                                                                                                                         https://mor.nlm.nih.gov/RxNav/search?searchBy=RXCUI&searchTerm=1665357
#> 49:                                                                                                                                                        https://www.medchemexpress.com/HY-14654
#> 50:                                                                                                                                                 https://www.probes-drugs.org/compound/PD002467
#> 51: https://www.ccdc.cam.ac.uk/structures/search?sid=UNICHEM&pid=csd:ACSALA,WIHROY,GARYEH,HAPCOU,HUPPOX,KEWNOQ,SIBYUA,XOJMOZ,ARIFOX,DIPJAQ,DISXOU,HUNJEH,TAZRAO,VUGMIT,YIRPEW,YOSMOI,DIFHOP,UTUCIW
#>                                                                                                                                                                                          sourceURL
#>                                                                                                                                                                                             <char>
#> 
#> $UniChem_Mappings
#> $UniChem_Mappings$UniChem.UCI
#> [1] 161671
#> 
#> $UniChem_Mappings$UniChem.InchiKey
#> [1] "BSYNRYMUTXBXSQ-UHFFFAOYSA-N"
#> 
#> $UniChem_Mappings$UniChem.Inchi
#> [1] "InChI=1S/C9H8O4/c1-6(10)13-8-5-3-2-4-7(8)9(11)12/h2-5H,1H3,(H,11,12)"
#> 
#> $UniChem_Mappings$UniChem.formula
#> [1] "C9H8O4"
#> 
#> $UniChem_Mappings$UniChem.connections
#> [1] "1-6(10)13-8-5-3-2-4-7(8)9(11)12"
#> 
#> $UniChem_Mappings$UniChem.hAtoms
#> [1] "2-5H,1H3,(H,11,12)"
```
