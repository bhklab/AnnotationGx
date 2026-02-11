# Query HGNC BioMart by Gene Symbols

Retrieves data from the HGNC BioMart for a list of gene symbols.

## Usage

``` r
query_hgnc_by_genes(genes, attributes, mart_name = NULL, dataset_name = NULL)
```

## Arguments

- genes:

  Character vector of HGNC gene symbols.

- attributes:

  Character vector of attribute display names to return.

- mart_name:

  Character, optional name or display name of the mart to use (default:
  first available mart).

- dataset_name:

  Character, optional name or display name of the dataset to use
  (default: first available dataset).

## Value

A data.table with results for matching gene symbols and attributes.

## Details

This function connects to the HGNC BioMart service and retrieves
specified attributes for a list of gene symbols. It automatically
handles the selection of marts and datasets based on provided
parameters.
