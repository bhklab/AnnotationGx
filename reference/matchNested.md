# Match inside nested elements

Match inside nested elements

## Usage

``` r
matchNested(x, table, ..., keep_duplicates = FALSE)

# S4 method for class 'character,list'
matchNested(x, table, keep_duplicates)

# S4 method for class 'numeric,list'
matchNested(x, table, keep_duplicates)

# S4 method for class 'character,data.table'
matchNested(x, table, keep_duplicates)

# S4 method for class 'numeric,data.table'
matchNested(x, table, keep_duplicates)

# S4 method for class 'character,data.frame'
matchNested(x, table, keep_duplicates)

# S4 method for class 'character,data.frame'
matchNested(x, table, keep_duplicates)
```

## Arguments

- x:

  The values to be matched.

- table:

  The values to be matched against. Applies across rows for `DataFrame`
  method.

- ...:

  Additional arguments to be passed to the method.

- keep_duplicates:

  A logical value indicating whether to keep duplicates.

## Value

`integer`. A positional vector corresponding to values defined in
`table` the same size as `x`.

## Details

Intentionally only performs exact matching. Refer to `filterNested`
function for partial matching support with regular expressions.

## Examples

``` r
showMethods("matchNested")
#> Function: matchNested (package AnnotationGx)
#> x="character", table="data.frame"
#> x="character", table="data.table"
#> x="character", table="list"
#> x="numeric", table="data.table"
#> x="numeric", table="list"
#> 
```
