# Standardize Names

This function takes a character vector and standardizes the names by
converting them to lowercase, removing any trailing information after a
comma, removing any information within square brackets or parentheses,
removing any non-alphanumeric characters, replacing empty names with
"invalid", and converting the names to uppercase.

## Usage

``` r
standardize_names(object)
```

## Arguments

- object:

  A character vector containing the names to be standardized.

## Value

A character vector with the standardized names.

## Examples

``` r
standardize_names(c("John Doe", "Jane Smith (Manager)", "Alice, PhD"))
#> [1] "JOHNDOE"   "JANESMITH" "ALICE"    
# Output: [1] "JOHNDOE" "JANESMITH" "ALICE"
```
