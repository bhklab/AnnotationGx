# Get annotation headings (name only) based on type and heading criteria.

Get annotation headings (name only) based on type and heading criteria.

## Usage

``` r
getPubchemAnnotationHeadings(type = "all", heading = NULL)
```

## Arguments

- type:

  The type of annotation headings to retrieve. Options include
  "Compound", "Gene", "Taxonomy", "Element", "Assay", "Protein", "Cell",
  "Pathway", or "all" (default).

- heading:

  The specific heading to filter the results by. Defaults to NULL, which
  retrieves all headings.

## Value

A `data.table` containing the annotation headings and types.

## Examples

``` r
getPubchemAnnotationHeadings()
#>               Heading     Type
#>                <char>   <char>
#>   1:  11B NMR Spectra Compound
#>   2:  13C NMR Spectra Compound
#>   3:  15N NMR Spectra Compound
#>   4:  17O NMR Spectra Compound
#>   5:  19F NMR Spectra Compound
#>  ---                          
#> 679: Wiley References Compound
#> 680:      WormBase ID     Gene
#> 681:      WormBase ID  Protein
#> 682:  Xenbase Gene ID     Gene
#> 683:          ZFIN ID     Gene
getPubchemAnnotationHeadings(type = "Compound")
#>                                        Heading     Type
#>                                         <char>   <char>
#>   1:                           11B NMR Spectra Compound
#>   2:                           13C NMR Spectra Compound
#>   3:                           15N NMR Spectra Compound
#>   4:                           17O NMR Spectra Compound
#>   5:                           19F NMR Spectra Compound
#>  ---                                                   
#> 513: Volatilization from Water/Soil (Complete) Compound
#> 514:                   WHO Essential Medicines Compound
#> 515:                                  Wikidata Compound
#> 516:                                 Wikipedia Compound
#> 517:                          Wiley References Compound
getPubchemAnnotationHeadings(heading = "ChEMBL*")
#>                Heading     Type
#>                 <char>   <char>
#> 1: ChEMBL Cell Line ID     Cell
#> 2:           ChEMBL ID Compound
#> 3:    ChEMBL Target ID  Protein
getPubchemAnnotationHeadings(type = "Compound", heading = "ChEMBL*")
#>      Heading     Type
#>       <char>   <char>
#> 1: ChEMBL ID Compound
```
