# Get available Oncotree versions

This function retrieves the available versions of Oncotree.

## Usage

``` r
getOncotreeVersions()
```

## Value

A `data.table` containing available Oncotree versions.

## Examples

``` r
# Requires internet connection to Oncotree API
if (interactive()) {
  getOncotreeVersions()
}
```
