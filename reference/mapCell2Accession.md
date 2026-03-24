# Map Cell Line IDs to Accession Numbers

This function maps cell line IDs to accession numbers using the
Cellosaurus database.

## Usage

``` r
mapCell2Accession(
  ids,
  numResults = 10000,
  from = "idsy",
  sort = "ac",
  keep_duplicates = FALSE,
  fuzzy = FALSE,
  query_only = FALSE,
  raw = FALSE,
  parsed = TRUE,
  include_query = TRUE,
  ...
)
```

## Arguments

- ids:

  A character vector of cell line IDs.

- numResults:

  The maximum number of results to return for each query. Default is
  1000.

- from:

  The type of input IDs. Possible values are "idsy" (default), "ac",
  "id", "sy", and "misspelling".

- sort:

  The sorting order of the results. Possible values are "ac" (default),
  "id", "sy", and "misspelling".

- keep_duplicates:

  Logical indicating whether to keep duplicate results. Default is
  FALSE.

- fuzzy:

  Logical indicating whether to perform a fuzzy search. Default is
  FALSE.

- query_only:

  Logical indicating whether to return only the query URLs. Default is
  FALSE.

- raw:

  Logical indicating whether to return the raw HTTP responses. Default
  is FALSE.

- parsed:

  Logical indicating whether to parse the response text. Default is
  TRUE.

- include_query:

  Logical indicating whether to include the `query*` columns (e.g.
  `query`, `query:ac`) in the returned result. Default is TRUE.

- ...:

  Currently unused. Reserved for future extensions.

## Value

A data.table containing the mapped cell line IDs and accession numbers.
When `parsed = FALSE`, the returned table also includes Cellosaurus
metadata columns that have been renamed to user-friendly titles (for
example, `sy` becomes `synonyms`).

## Examples

``` r
mapCell2Accession(ids = c("A549", "MCF7"))
#>    cellLineName accession  query
#>          <char>    <char> <char>
#> 1:        A-549 CVCL_0023   A549
#> 2:        MCF-7 CVCL_0031   MCF7
```
