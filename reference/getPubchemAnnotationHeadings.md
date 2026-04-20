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
#>                     x
#>                <char>
#> 1: PUGREST.ServerBusy
getPubchemAnnotationHeadings(type = "Compound")
#> [13:58:02][WARNING][AnnotationGx::getPubchemAnnotationHeadings]  No headings found for type: ` Compound ` and heading: `  `.
#> Try getPubchemAnnotationHeadings(type = 'all') for available headings and types 
#> Empty data.table (0 rows and 1 cols): x
getPubchemAnnotationHeadings(heading = "ChEMBL*")
#> [13:58:02][WARNING][AnnotationGx::getPubchemAnnotationHeadings]  No headings found for type: ` all ` and heading: ` ChEMBL* `.
#> Try getPubchemAnnotationHeadings(type = 'all') for available headings and types 
#> Empty data.table (0 rows and 1 cols): x
getPubchemAnnotationHeadings(type = "Compound", heading = "ChEMBL*")
#> [13:58:02][WARNING][AnnotationGx::getPubchemAnnotationHeadings]  No headings found for type: ` Compound ` and heading: ` ChEMBL* `.
#> Try getPubchemAnnotationHeadings(type = 'all') for available headings and types 
#> Empty data.table (0 rows and 1 cols): x
```
