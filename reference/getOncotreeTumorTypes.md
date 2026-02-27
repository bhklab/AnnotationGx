# Get the tumor types from the Oncotree database.

This function retrieves the tumor types from the Oncotree database.

## Usage

``` r
getOncotreeTumorTypes()
```

## Value

A `data.table` containing the tumor types from the Oncotree database.

## Examples

``` r
# Requires internet connection to Oncotree API
if (interactive()) {
  getOncotreeTumorTypes()
}
```
