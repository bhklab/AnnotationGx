# Querying PubChem

## Introduction to PubChem APIs

PubChem is a database of chemical molecules and their biological
activities. It is a part of the National Center for Biotechnology
Information (NCBI), which is a part of the National Institutes of Health
(NIH). PubChem provides a set of APIs to query its database. The
`AnnotationGx` package provides a set of functions to query PubChem
using these APIs.

The first of these APIs is the `PubChem PUG REST API` which is designed
to - make specific queries based on some input *identifier* and return
data which PubChem has labelled or computed internally \[1\]. - This API
is useful for querying information about a specific chemical compound
such as getting the standardized PubChem identifier (CID) for a given
chemical name or smiles string, or getting the chemical structure for a
given CID. - It provides access to a wide range of data including
chemical properties, bioassay data, and chemical classification data,
given a specific identifier.

The second API is the `PubChem PUG VIEW API` which is designed to: -
give accesse to aggregated annotations for a given chemical compound
\[3\] that is mapped to their data, but not curated by PubChem itself. -
i.e it provides access to annotations from external sources such as
UniProt, ChEBI, and ChEMBL, given a specific identifier.

### Licensing

PubChem data usage is described by the [NCBI Website and Data Usage
Policies](https://www.ncbi.nlm.nih.gov/home/about/policies/). Source:
<https://www.ncbi.nlm.nih.gov/home/about/policies/>

## Setup

``` r
library(AnnotationGx)
```

## Mapping from chemical name to PubChem CID

The main function that is provided by the package is `mapCompound2CID`.

``` r
mapCompound2CID("aspirin")
#>       name  cids
#>     <char> <int>
#> 1: aspirin  2244
```

You can pass in a list of compound names to get the CIDs for all of them
at once.

``` r
drugs <- c(
  "Aspirin",
  "Erlotinib",
  "Acadesine",
  "Camptothecin",
  "Vincaleukoblastine",
  "Cisplatin"
)

mapCompound2CID(drugs)
#>                  name    cids
#>                <char>   <int>
#> 1:            Aspirin    2244
#> 2:          Erlotinib  176870
#> 3:          Acadesine   17513
#> 4:       Camptothecin   24360
#> 5: Vincaleukoblastine   13342
#> 6:          Cisplatin 5460033
#> 7:          Cisplatin 5702198
```

It is possible for names to multimap to CIDs. This is the case for
‘*Vincaleukoblastine*’ in the above query. In cases of multimapping,
usually the first entry has the highest similarity to the requested
drug. To subset to only the first occurrence of each of drug name use
the `first = TRUE` argument:

``` r
mapCompound2CID(drugs, first = TRUE)
#> Waiting 30s for retry backoff ■■                              
#> Waiting 30s for retry backoff ■■■                             
#> Waiting 30s for retry backoff ■■■■■■                          
#> Waiting 30s for retry backoff ■■■■■■■■■                       
#> Waiting 30s for retry backoff ■■■■■■■■■■■■                    
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■                 
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■              
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■           
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■        
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■     
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 
#>                  name    cids
#>                <char>   <int>
#> 1:            Aspirin    2244
#> 2:          Erlotinib  176870
#> 3:          Acadesine   17513
#> 4:       Camptothecin   24360
#> 5: Vincaleukoblastine   13342
#> 6:          Cisplatin 5460033
```

In the case that a compound cannot be mapped, `NA` will be returned and
a warning will be issued.

``` r
(result <- mapCompound2CID(
  c(drugs, "non existent compound", "another bad compound"),
  first = TRUE
))
#> Waiting 30s for retry backoff ■■                              
#> Waiting 30s for retry backoff ■■■■■                           
#> Waiting 30s for retry backoff ■■■■■■■■                        
#> Waiting 30s for retry backoff ■■■■■■■■■■■                     
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■                  
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■               
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■            
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■         
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■      
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   
#> Waiting 30s for retry backoff ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■ 
#> [20:56:34][WARNING][AnnotationGx::getPubchemCompound]  Some queries failed. See the 'failed' object for details. 
#>                     name    cids
#>                   <char>   <int>
#> 1:               Aspirin    2244
#> 2:             Erlotinib  176870
#> 3:             Acadesine   17513
#> 4:          Camptothecin   24360
#> 5:    Vincaleukoblastine   13342
#> 6:             Cisplatin 5460033
#> 7: non existent compound      NA
#> 8:  another bad compound      NA

failed <- attributes(result)$failed

# get the list of failed inputs
names(failed)
#> [1] "non existent compound" "another bad compound"

# get the error message for the failed input
print(failed[1])
#> $`non existent compound`
#> $`non existent compound`$Code
#> [1] "PUGREST.NotFound"
#> 
#> $`non existent compound`$Message
#> [1] "No CID found"
#> 
#> $`non existent compound`$Details
#> [1] "No CID found that matches the given name"
```

## Mapping from PubChem CID to Properties

Once CIDs are obtained, they can be used to query the properties of the
compound. To view the available properties from Pubchem, use the
`getPubchemProperties` function.

``` r
getPubchemProperties()
#>                         name         type
#>                       <char>       <char>
#>  1:                      CID          int
#>  2:         MolecularFormula       string
#>  3:          MolecularWeight       string
#>  4:                   SMILES       string
#>  5:       ConnectivitySMILES       string
#>  6:                    InChI       string
#>  7:                 InChIKey       string
#>  8:                IUPACName       string
#>  9:                    XLogP       double
#> 10:                ExactMass       string
#> 11:         MonoisotopicMass       string
#> 12:                     TPSA       double
#> 13:               Complexity          int
#> 14:                   Charge          int
#> 15:          HBondDonorCount          int
#> 16:       HBondAcceptorCount          int
#> 17:       RotatableBondCount          int
#> 18:           HeavyAtomCount          int
#> 19:         IsotopeAtomCount          int
#> 20:          AtomStereoCount          int
#> 21:   DefinedAtomStereoCount          int
#> 22: UndefinedAtomStereoCount          int
#> 23:          BondStereoCount          int
#> 24:   DefinedBondStereoCount          int
#> 25: UndefinedBondStereoCount          int
#> 26:        CovalentUnitCount          int
#> 27:                 Volume3D       double
#> 28:      XStericQuadrupole3D       double
#> 29:      YStericQuadrupole3D       double
#> 30:      ZStericQuadrupole3D       double
#> 31:           FeatureCount3D          int
#> 32:   FeatureAcceptorCount3D          int
#> 33:      FeatureDonorCount3D          int
#> 34:      FeatureAnionCount3D          int
#> 35:     FeatureCationCount3D          int
#> 36:       FeatureRingCount3D          int
#> 37: FeatureHydrophobeCount3D          int
#> 38:     ConformerModelRMSD3D       double
#> 39:    EffectiveRotorCount3D       double
#> 40:         ConformerCount3D          int
#> 41:            Fingerprint2D base64Binary
#> 42:                    Title       string
#> 43:              PatentCount          int
#> 44:        PatentFamilyCount          int
#> 45:          LiteratureCount          int
#> 46:          AnnotationTypes       string
#> 47:      AnnotationTypeCount          int
#> 48:         SourceCategories       string
#>                         name         type
#>                       <char>       <char>
```

After deciding which properties to query, you can use the
`mapCID2Properties` function to get the properties for a specific CID.

``` r
properties <- c("Title", "MolecularFormula", "InChIKey", "MolecularWeight")

# Need to remove NA values from the query as they will cause an error
result[!is.na(cids), mapCID2Properties(ids = cids, properties = properties)]
#>        CID MolecularFormula MolecularWeight                    InChIKey
#>      <int>           <char>          <char>                      <char>
#> 1:    2244           C9H8O4          180.16 BSYNRYMUTXBXSQ-UHFFFAOYSA-N
#> 2:  176870       C22H23N3O4           393.4 AAKJLRGGTJKAMG-UHFFFAOYSA-N
#> 3:   17513        C9H14N4O5          258.23 RTRQQBHATOEIAF-UUOKFMHZSA-N
#> 4:   24360       C20H16N2O4           348.4 VSJKWCGYPAHWDS-FQEVSTJZSA-N
#> 5:   13342       C46H58N4O9           811.0 JXLYSJRDGCGARV-CFWMRBGOSA-N
#> 6: 5460033        Cl2H6N2Pt          300.05 LXZZYRPGZAFOLE-UHFFFAOYSA-L
#>           Title
#>          <char>
#> 1:      Aspirin
#> 2:    Erlotinib
#> 3:    Acadesine
#> 4: Camptothecin
#> 5:  Vinblastine
#> 6:    Cisplatin
```

## Mapping from PubChem CID to Annotations

Pubchem’s VIEW API provides access to annotations from external sources
such as UniProt, ChEBI, and ChEMBL, given a specific identifier. Before
querying annotations, we need to use the exact heading we want to query.

You can use the `getPubchemAnnotationHeadings` function to get the
available annotation headings and types.

#### Get ALL available annotation headings:

``` r
getPubchemAnnotationHeadings()
#>              Heading     Type
#>               <char>   <char>
#>   1: 11B NMR Spectra Compound
#>   2: 13C NMR Spectra Compound
#>   3: 15N NMR Spectra Compound
#>   4: 17O NMR Spectra Compound
#>   5: 19F NMR Spectra Compound
#>  ---                         
#> 697:       Withdrawn Compound
#> 698:     WormBase ID     Gene
#> 699:     WormBase ID  Protein
#> 700: Xenbase Gene ID     Gene
#> 701:         ZFIN ID     Gene
```

#### Get annotation headings for a specific type:

``` r
getPubchemAnnotationHeadings(type = "Compound")
#>                      Heading     Type
#>                       <char>   <char>
#>   1:         11B NMR Spectra Compound
#>   2:         13C NMR Spectra Compound
#>   3:         15N NMR Spectra Compound
#>   4:         17O NMR Spectra Compound
#>   5:         19F NMR Spectra Compound
#>  ---                                 
#> 529: WHO Essential Medicines Compound
#> 530:                Wikidata Compound
#> 531:               Wikipedia Compound
#> 532:        Wiley References Compound
#> 533:               Withdrawn Compound
```

#### Get annotation headings for a specific heading:

``` r
getPubchemAnnotationHeadings(heading = "ChEMBL ID")
#>      Heading     Type
#>       <char>   <char>
#> 1: ChEMBL ID Compound
```

#### Get annotation headings for a specific type **and** heading:

``` r
getPubchemAnnotationHeadings(type = "Compound", heading = "CAS")
#>           Heading     Type
#>            <char>   <char>
#> 1:            CAS Compound
#> 2: Deprecated CAS Compound
#> 3:    Related CAS Compound
```

#### Query annotations for a specific CID and heading

We can then use the heading to query the annotations for a specific CID.

``` r
result[!is.na(cids), CAS := annotatePubchemCompound(cids, "CAS")]
result
#>                     name    cids                                CAS
#>                   <char>   <int>                             <char>
#> 1:               Aspirin    2244                            50-78-2
#> 2:             Erlotinib  176870                        183321-74-6
#> 3:             Acadesine   17513                          2627-69-2
#> 4:          Camptothecin   24360                          7689-03-4
#> 5:    Vincaleukoblastine   13342                           865-21-4
#> 6:             Cisplatin 5460033 15663-27-1; 26035-31-4; 14913-33-8
#> 7: non existent compound      NA                               <NA>
#> 8:  another bad compound      NA                               <NA>
```

## References

1.  PUG REST. PubChem Docs \[website\]. Retrieved from
    <https://pubchemdocs.ncbi.nlm.nih.gov/pug-rest>.
2.  Kim S, Thiessen PA, Cheng T, Yu B, Bolton EE. An update on PUG-REST:
    RESTful interface for programmatic access to PubChem. Nucleic Acids
    Res. 2018 July 2; 46(W1):W563-570. <doi:10.1093/nar/gky294>.
3.  PUG VIEW. PubChem Docs \[webiste\]. Retrieved from
    <https://pubchemdocs.ncbi.nlm.nih.gov/pug-view>.
4.  Kim S, Thiessen PA, Cheng T, Zhang J, Gindulyte A, Bolton EE.
    PUG-View: programmatic access to chemical annotations integrated in
    PubChem. J Cheminform. 2019 Aug 9; 11:56.
    <doi:10.1186/s13321-019-0375-2>.

``` r
sessionInfo()
#> R version 4.5.3 (2026-03-11)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.4 LTS
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
#>  [1] crayon_1.5.3        cli_3.6.6           knitr_1.51         
#>  [4] rlang_1.2.0         xfun_0.57           textshaping_1.0.5  
#>  [7] jsonlite_2.0.0      data.table_1.18.2.1 glue_1.8.0         
#> [10] backports_1.5.1     htmltools_0.5.9     ragg_1.5.2         
#> [13] sass_0.4.10         rappdirs_0.3.4      rmarkdown_2.31     
#> [16] evaluate_1.0.5      jquerylib_0.1.4     fastmap_1.2.0      
#> [19] yaml_2.3.12         lifecycle_1.0.5     httr2_1.2.2        
#> [22] memoise_2.0.1       compiler_4.5.3      fs_2.0.1           
#> [25] systemfonts_1.3.2   digest_0.6.39       R6_2.6.1           
#> [28] parallel_4.5.3      curl_7.0.0          magrittr_2.0.5     
#> [31] bslib_0.10.0        checkmate_2.3.4     withr_3.0.2        
#> [34] tools_4.5.3         xml2_1.5.2          pkgdown_2.2.0      
#> [37] cachem_1.1.0        desc_1.4.3
```
