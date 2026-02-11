# Clean character strings by removing special characters and formatting.

This function takes a character string as input and performs several
cleaning operations to remove special characters, formatting, and
unwanted substrings. The cleaned string is then returned as the output.

## Usage

``` r
cleanCharacterStrings(name, space_action = "")
```

## Arguments

- name:

  A character string to be cleaned.

- space_action:

  A character vector specifying the actions to be taken for space
  characters. One of c("", "-", " ").

## Value

The cleaned character string.

## Examples

``` r
cleanCharacterStrings("Cisplatin: 1 mg/mL (1.5 mM); 5 mM in DMSO")
#> [1] "CISPLATIN"
```
