# Annotate Cell Accession

This function takes a Cellosaurus accession and returns annotations for
the cell line.

## Usage

``` r
annotateCellAccession(
  accessions,
  to = c("id", "ac", "hi", "sy", "ca", "sx", "ag", "di", "derived-from-site",
    "misspelling", "dt"),
  query_only = FALSE,
  raw = FALSE
)
```

## Arguments

- accessions:

  The Cellosaurus accession to annotate.

- to:

  A character vector specifying the types of annotations to retrieve.
  Possible values include "id", "ac", "hi", "sy", "ca", "sx", "ag",
  "di", "derived-from-site", "misspelling", and "dt".

- query_only:

  A logical value indicating whether to only return the query string.

- raw:

  A logical value indicating whether to return the raw response.

## Value

A data frame containing the annotations for the cell line.

## Examples

``` r
annotateCellAccession("CVCL_0031")
#>    cellLineName accession         category
#>          <char>    <char>           <char>
#> 1:        MCF-7 CVCL_0031 Cancer cell line
#>                                                      date ageAtSampling
#>                                                    <char>        <char>
#> 1: Created: 04-04-12; Last updated: 27-11-25; Version: 54           69Y
#>    sexOfCell
#>       <char>
#> 1:    Female
#>                                                               synonyms
#>                                                                 <list>
#> 1: MCF 7,MCF.7,MCF7,Michigan Cancer Foundation-7,ssMCF-7,ssMCF7,...[9]
#>     diseases crossReferences hierarchy  comments
#>       <list>          <char>    <char>    <list>
#> 1: <list[1]>            <NA>      <NA> <list[2]>
annotateCellAccession("CVCL_0031", to = c("id", "ac", "hi", "sy"))
#>    cellLineName accession ageAtSampling sexOfCell
#>          <char>    <char>        <char>    <char>
#> 1:        MCF-7 CVCL_0031          <NA>      <NA>
#>                                                               synonyms diseases
#>                                                                 <list>   <char>
#> 1: MCF 7,MCF.7,MCF7,Michigan Cancer Foundation-7,ssMCF-7,ssMCF7,...[9]     <NA>
#>    crossReferences hierarchy comments
#>             <char>    <char>   <char>
#> 1:            <NA>      <NA>     <NA>
```
