# Annotating CTRP Treatments

## Introduction

This vignette compares annotating CTRP-provided treatment ids to PubChem
CIDs and CTD information.

Whereas the PubChem CID is a unique identifier for a compound, the
PubChem API does not easily map treatment names to CIDs, atleast not in
a way that easy for commonly misnamed treatments. Specifically, for the
**CTRP** treatment names (n=545), the PubChem API does not correctly map
all of them to PubChem CIDs.

It is an investigation to see which of the methods might map more
compounds

``` r
library(AnnotationGx)

data(CTRP_treatmentMetadata)
```

``` r
# get a random row from the CTRP_treatmentMetadata

treatment <- CTRP_treatmentMetadata[1, CTRP.treatmentid]
sprintf("CTRP treatment id : %s", treatment)
#> [1] "CTRP treatment id : CIL55"

# map the treatment to a CID using PubChem
mapCompound2CID(treatment)
#>      name    cids
#>    <char>   <int>
#> 1:  CIL55 6623618
```

### Annotating using PubChem

``` r
(compounds_to_cids <-
  CTRP_treatmentMetadata[
    1:10,
    AnnotationGx::mapCompound2CID(
      names = CTRP.treatmentid,
      first = TRUE
    )
  ]
)
failed <-
  attributes(compounds_to_cids)$failed |>
  names()
```

``` r
failed <- unique(CTRP_treatmentMetadata[CTRP.treatmentid %in% failed, ])

failed[, CTRP.treatmentid_CLEANED := cleanCharacterStrings(CTRP.treatmentid)]

(failed_to_cids <-
  failed[
    ,
    AnnotationGx::mapCompound2CID(
      names = CTRP.treatmentid_CLEANED,
      first = TRUE
    )
  ]
)
failed_again <-
  attributes(failed_to_cids)$failed |>
  names()
```

``` r
failed_dt <- merge(failed_to_cids[!is.na(cids), ], failed, by.x = "name", by.y = "CTRP.treatmentid_CLEANED", all.x = FALSE)
failed_dt$name <- NULL

successful_dt <- merge(CTRP_treatmentMetadata, compounds_to_cids[!is.na(cids), ], by.x = "CTRP.treatmentid", by.y = "name", all.x = FALSE)

mapped_PubChem <- data.table::rbindlist(list(successful_dt, failed_dt), use.names = TRUE, fill = TRUE)
```
