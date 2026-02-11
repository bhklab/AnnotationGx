# Split a character vector into a matrix based on a delimiter

This function splits a character vector into a matrix based on a
specified delimiter. It can handle both finite and infinite splits.

## Usage

``` r
strSplit(x, split, fixed = TRUE, n = Inf)
```

## Arguments

- x:

  A character vector to be split

- split:

  A character string specifying the delimiter

- fixed:

  A logical value indicating whether the delimiter should be treated as
  a fixed string

- n:

  An integer specifying the maximum number of splits to be performed

## Value

A matrix where each row represents a split element

## Examples

``` r
strSplit("Hello,World", ",")
#>      [,1]    [,2]   
#> [1,] "Hello" "World"
# Output:
#      [,1]    [,2]   
# [1,] "Hello" "World"
```
