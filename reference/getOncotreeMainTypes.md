# Get the main types from the Oncotree database.

This function retrieves the main types from the Oncotree database.

## Usage

``` r
getOncotreeMainTypes()
```

## Value

A `data.table` containing the main types from the Oncotree database.

## Examples

``` r
# Requires internet connection to Oncotree API
if (interactive()) {
  getOncotreeMainTypes()
}
```
