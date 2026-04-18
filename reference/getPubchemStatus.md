# Retrieves the status of a PubChem request

This function sends a request to PubChem to retrieve the status of a
given URL. It returns the status code and, if specified, the parsed
information from the response.

## Usage

``` r
getPubchemStatus(
  returnMessage = FALSE,
  printMessage = TRUE,
  url = "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/Aspirin/cids/JSON"
)
```

## Arguments

- returnMessage:

  Logical indicating whether to return the parsed information from the
  response.

- printMessage:

  Logical indicating whether to print the status message.

- url:

  The URL to send the request to. Default is
  "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/Aspirin/cids/JSON".

## Value

The status code of the response. If `returnMessage` is `TRUE`, the
parsed information from the response is also returned.

## Examples

``` r
getPubchemStatus()
#> Throttling status:
#> Request Count status: Green (1%)
#> Request Time status: Green (0%)
#> Service status: Green (27%)
getPubchemStatus(returnMessage = TRUE)
#> Throttling status:
#> Request Count status: Green (0%)
#> Request Time status: Green (0%)
#> Service status: Green (40%)
#> $request_count
#> $request_count$status
#> [1] "Green"
#> 
#> $request_count$percent
#> [1] 0
#> 
#> 
#> $request_time
#> $request_time$status
#> [1] "Green"
#> 
#> $request_time$percent
#> [1] 0
#> 
#> 
#> $service
#> $service$status
#> [1] "Green"
#> 
#> $service$percent
#> [1] 40
#> 
#> 
getPubchemStatus(printMessage = FALSE)
```
