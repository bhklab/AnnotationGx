# Get the list of sources in UniChem.

Get the list of sources in UniChem.

## Usage

``` r
getUnichemSources(all_columns = FALSE)
```

## Arguments

- all_columns:

  `boolean` Whether to return all columns. Defaults to FALSE.

  Returns a `data.table` with the following columns:

  - `CompoundCount` (integer): Total of compounds provided by that
    source

  - `BaseURL` (string): Source Base URL for compounds

  - `Description` (string): Source database description

  - `LastUpdated` (string): Date in which the source database was last
    updated

  - `Name` (string): Short name of the source database

  - `NameLabel` (string): Machine readable label name of the source
    database

  - `NameLong` (string): Full name of the source database

  - `SourceID` (integer): Unique ID for the source database

  - `Details` (string): Notes about the source

  - `ReleaseDate` (string): Date in which the source database was
    released

  - `ReleaseNumber` (integer): Release number of the source database
    data stored in UniChEM

  - `URL` (string): Main URL for the source

  - `UpdateComments` (string): Notes about the update process of that
    source to UniChEM

## Value

A data.table with the list of sources in UniChem.

## Examples

``` r
# Requires internet connection to UniChem
if (interactive()) {
  getUnichemSources()
}
```
