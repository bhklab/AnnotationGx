# Map compound names to PubChem CIDs

This function maps compound names to PubChem CIDs using the PubChem REST
API.

## Usage

``` r
mapCompound2CID(names, first = FALSE, ...)
```

## Arguments

- names:

  A character vector of compound names.

- first:

  Logical indicating whether to return only the first CID for each
  compound name (default is FALSE).

- ...:

  Additional arguments to be passed to the getPubchemCompound function.

## Value

A character vector of PubChem CIDs.

## Examples

``` r
mapCompound2CID(c("aspirin", "caffeine"))
#>        name  cids
#>      <char> <int>
#> 1:  aspirin  2244
#> 2: caffeine  2519
```
