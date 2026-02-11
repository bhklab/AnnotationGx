# Map PubChem Compound IDs to Properties

This function maps PubChem Compound IDs to specified properties using
the PubChem REST API. See `getPubchemProperties` for a list of available
properties.

## Usage

``` r
mapCID2Properties(ids, properties, ...)
```

## Arguments

- ids:

  A vector of PubChem Compound IDs.

- properties:

  A vector of property names to retrieve for each compound.

- ...:

  Additional arguments to be passed to the `getPubchemCompound`
  function.

## Value

A data frame containing the mapped properties for each compound.

## Examples

``` r
mapCID2Properties(ids = c(123, 456), properties = c("MolecularWeight", "CanonicalSMILES"))
#>      CID MolecularWeight   ConnectivitySMILES
#>    <int>          <char>               <char>
#> 1:   123          144.18   C(CC(=O)N)CN=C(N)N
#> 2:   456          132.08 C(=O)(C(=O)O)NC(=O)N
```
