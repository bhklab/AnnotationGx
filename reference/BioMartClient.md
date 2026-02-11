# BioMart API Client

Client class for interacting with the BioMart REST API.

## Details

This class provides methods for querying BioMart REST API endpoints,
retrieving information about available marts, datasets, attributes, and
filters.

## Public fields

- `base_url`:

  Base URL of the BioMart service

- `path`:

  Path to the BioMart API on the server

## Methods

### Public methods

- [`BioMartClient$new()`](#method-BioMartClient-new)

- [`BioMartClient$get_marts()`](#method-BioMartClient-get_marts)

- [`BioMartClient$get_datasets()`](#method-BioMartClient-get_datasets)

- [`BioMartClient$get_attributes()`](#method-BioMartClient-get_attributes)

- [`BioMartClient$get_filters()`](#method-BioMartClient-get_filters)

- [`BioMartClient$clone()`](#method-BioMartClient-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new BioMartClient

#### Usage

    BioMartClient$new(base_url, path = "/biomart")

#### Arguments

- `base_url`:

  Character, the base URL of the BioMart service

- `path`:

  Character, the API path, defaults to "/biomart"

#### Returns

A new BioMartClient object

------------------------------------------------------------------------

### Method `get_marts()`

Retrieve available marts from the BioMart service

#### Usage

    BioMartClient$get_marts()

#### Returns

List of MartInfo objects

------------------------------------------------------------------------

### Method `get_datasets()`

Retrieve available datasets for a given mart

#### Usage

    BioMartClient$get_datasets(mart)

#### Arguments

- `mart`:

  MartInfo object, the mart to query

#### Returns

List of DatasetInfo objects

------------------------------------------------------------------------

### Method `get_attributes()`

Retrieve available attributes for a given dataset

#### Usage

    BioMartClient$get_attributes(dataset)

#### Arguments

- `dataset`:

  DatasetInfo object, the dataset to query

#### Returns

List of attribute information

------------------------------------------------------------------------

### Method `get_filters()`

Retrieve available filters for a given dataset

#### Usage

    BioMartClient$get_filters(dataset)

#### Arguments

- `dataset`:

  DatasetInfo object, the dataset to query

#### Returns

List of filter information

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    BioMartClient$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a client for Ensembl BioMart
client <- BioMartClient$new("https://www.ensembl.org")

# Get available marts
marts <- client$get_marts()

# Select a mart and get its datasets
ensembl <- marts[[1]]
datasets <- client$get_datasets(ensembl)
} # }
```
