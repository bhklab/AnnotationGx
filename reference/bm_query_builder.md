# Build BioMart Query XML

Build BioMart Query XML

## Usage

``` r
bm_query_builder(
  dataset,
  filters = list(),
  attributes = character(),
  client_name = "biomartclient",
  processor = "TSV",
  header = TRUE,
  limit = -1
)
```

## Arguments

- dataset:

  A DatasetInfo object representing the BioMart dataset to query

- filters:

  List of filters, either:

  - Named list where names are filter names and values are filter values

  - List of FilterInfo objects with added 'value' field

- attributes:

  AttributeSet, list of AttributeInfo objects, or character vector
  specifying the attributes to retrieve

- client_name:

  Character, name of client for query identification (default:
  "biomartclient")

- processor:

  Character, output format processor type (default: "TSV")

- header:

  Logical, whether to include header row in results (default: TRUE)

- limit:

  Integer, maximum number of rows to return, -1 for unlimited (default:
  -1)

## Value

Character string containing the formatted XML BioMart query
