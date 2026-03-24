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

### Licensing

UniChem is provided under the [EMBL-EBI Terms of
Use](https://www.ebi.ac.uk/about/terms-of-use/). Source:
<https://www.ebi.ac.uk/licencing/>

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
#>  1: probes_and_drugs       49
#>  2:          pubchem       22
#>  3:        bindingdb       31
#>  4:        lipidmaps       33
#>  5:           fdasrs       14
#>  6:      nmrshiftdb2       24
#>  7:      drugcentral       34
#>  8:           chembl        1
#>  9:         rcsb_pdb        3
#> 10:             rhea       38
#> 11:       surechembl       15
#> 12:           brenda       37
#> 13:      swisslipids       41
#> 14:             CCDC       50
#> 15:          molport       28
#> 16:           gtopdb        4
#> 17:            chebi        7
#> 18:         drugbank        2
#> 19:             hmdb       18
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
#>              compoundID             Name
#>                  <char>           <char>
#>  1:            CHEMBL25           chembl
#>  2:             DB00945         drugbank
#>  3:                 AIN         rcsb_pdb
#>  4:                4139           gtopdb
#>  5:                 AIN             pdbe
#>  6:         CHEBI:15365            chebi
#>  7:          R16CO5Y76E           fdasrs
#>  8:                1353       surechembl
#>  9:            29350479       surechembl
#> 10:         HMDB0001879             hmdb
#> 11:                2244          pubchem
#> 12: Molport-000-871-622          molport
#> 13:               22360        bindingdb
#> 14:                  74      drugcentral
#> 15:              159662           brenda
#> 16:                2261           brenda
#> 17:                3100           brenda
#> 18:               32748           brenda
#> 19:                4779           brenda
#> 20:                6476           brenda
#> 21:            PD002467 probes_and_drugs
#> 22:              ACSALA             CCDC
#>              compoundID             Name
#>                  <char>           <char>
#>                                        NameLong sourceID
#>                                          <char>    <int>
#>  1:                                      ChEMBL        1
#>  2:                                    DrugBank        2
#>  3:                                    RCSB PDB        3
#>  4:                       Guide to Pharmacology        4
#>  5:                 Protein Data Bank in Europe        5
#>  6:                                       ChEBI        7
#>  7: FDA/USP Substance Registration System (SRS)       14
#>  8:                                  SureChEMBL       15
#>  9:                                  SureChEMBL       15
#> 10:                                        HMDB       18
#> 11:                           PubChem Compounds       22
#> 12:                                     MolPort       28
#> 13:                                   BindingDB       31
#> 14:                                 DrugCentral       34
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
#>  1:                          https://www.ebi.ac.uk/chembldb/compound/inspect/CHEMBL25
#>  2:                                             https://go.drugbank.com/drugs/DB00945
#>  3:                                                   https://www.rcsb.org/ligand/AIN
#>  4:       https://www.guidetopharmacology.org/GRAC/LigandDisplayForward?ligandId=4139
#>  5:                 https://www.ebi.ac.uk/pdbe-srv/pdbechem/chemicalCompound/show/AIN
#>  6:                                           https://www.ebi.ac.uk/chebi/CHEBI:15365
#>  7:              https://d20b1koi85gdl2.cloudfront.net/uniisearch/srs/unii/R16CO5Y76E
#>  8:                                          https://www.surechembl.org/chemical/1353
#>  9:                                      https://www.surechembl.org/chemical/29350479
#> 10:                                       https://www.hmdb.ca/metabolites/HMDB0001879
#> 11:                                    https://pubchem.ncbi.nlm.nih.gov/compound/2244
#> 12:                         https://www.molport.com/shop/compound/Molport-000-871-622
#> 13: https://www.bindingdb.org/bind/chemsearch/marvin/MolStructure.jsp?monomerid=22360
#> 14:                                               https://drugcentral.org/drugcard/74
#> 15:                 https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=159662
#> 16:                   https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=2261
#> 17:                   https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=3100
#> 18:                  https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=32748
#> 19:                   https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=4779
#> 20:                   https://www.brenda-enzymes.org/ligand.php?brenda_ligand_id=6476
#> 21:                                   https://www.probes-drugs.org/compounds/PD002467
#> 22:           https://www.ccdc.cam.ac.uk/structures/search?sid=UNICHEM&pid=csd:ACSALA
#>                                                                             sourceURL
#>                                                                                <char>
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

``` r
sessionInfo()
#> R version 4.5.3 (2026-03-11)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.3 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] AnnotationGx_0.99.1
#> 
#> loaded via a namespace (and not attached):
#>  [1] cli_3.6.5           knitr_1.51          rlang_1.1.7        
#>  [4] xfun_0.57           textshaping_1.0.5   jsonlite_2.0.0     
#>  [7] data.table_1.18.2.1 glue_1.8.0          backports_1.5.0    
#> [10] htmltools_0.5.9     ragg_1.5.2          sass_0.4.10        
#> [13] rappdirs_0.3.4      rmarkdown_2.30      evaluate_1.0.5     
#> [16] jquerylib_0.1.4     fastmap_1.2.0       yaml_2.3.12        
#> [19] lifecycle_1.0.5     httr2_1.2.2         compiler_4.5.3     
#> [22] fs_2.0.0            systemfonts_1.3.2   digest_0.6.39      
#> [25] R6_2.6.1            curl_7.0.0          magrittr_2.0.4     
#> [28] bslib_0.10.0        checkmate_2.3.4     tools_4.5.3        
#> [31] pkgdown_2.2.0       cachem_1.1.0        desc_1.4.3
```
