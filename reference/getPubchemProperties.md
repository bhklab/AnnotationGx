# Retrieves the PubChem XML schema and extracts property information.

This function retrieves the PubChem XML schema from the specified URL
and extracts the property information from it. The property information
includes the name and type of each property.

## Usage

``` r
getPubchemProperties()
```

## Value

A data table containing the extracted property information.

## Examples

``` r
# Requires internet connection to PubChem
if (interactive()) {
  getPubchemProperties()
}
```
