# Unlists a nested list and removes NA values and duplicates.

This function takes a nested list as input and unlists it recursively.
It then removes any NA values and duplicates from the resulting vector.

## Usage

``` r
unlistNested(element)
```

## Arguments

- element:

  The nested list to be unlisted.

## Value

A vector with NA values and duplicates removed.

## Examples

``` r
nested_list <- list(a = list(1, 2, NA), b = list(3, 4, 5))
unlistNested(nested_list)
#> [1] 1 2 3 4 5
# Output: [1] 1 2 3 4 5
```
