# gdsc_sampleMetadata is some preprocessed sample metadata from the GDSC dataset

A preprocessed version of the sample metadata from the GDSC dataset.
This dataset is provided in the package to test the functionality of the
package. The original dataset can be downloaded from the CancerRxGene
website.

## Usage

``` r
data(GDSC_sampleMetadata)
```

## Format

A data table with 5 columns and 1001 rows.

- GDSC.Sample_Name:

  `char` The name of the cell line in the GDSC dataset.

- GDSC.COSMIC_ID:

  `int` The COSMIC ID of the cell line in the GDSC dataset.

## Source

https://www.cancerrxgene.org/

## Examples

``` r
data(GDSC_sampleMetadata)
head(GDSC_sampleMetadata)
#>    GDSC.Sample_Name GDSC.COSMIC_ID
#>              <char>          <num>
#> 1:             A253         906794
#> 2:         BB30-HNC         753531
#> 3:         BB49-HNC         753532
#> 4:              BHY         753535
#> 5:           BICR10        1290724
#> 6:           BICR22        1240121
```
