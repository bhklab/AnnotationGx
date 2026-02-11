# AnnotationGx

## Installation

You can install from github using:

``` r
remotes::install_github(
    "bhklab/AnnotationGx", 
    build_manual=TRUE, 
    build_vignettes=TRUE
)
```

## Example

First load the package:

``` r
library(AnnotationGx)
help(package="AnnotationGx")
```

Annotate cell line using cellosaurus:

``` r
name <- "A549"

mapCell2Accession(name)
```

Annotate drugs using PubChem:

``` r
drugs <- c(
  "Aspirin", "Erlotinib", "Acadesine", 
  "Camptothecin", "Vincaleukoblastine", "Cisplatin"
)

(compound_2_cids <- mapCompound2CID(drugs, first = TRUE))
mapCID2Properties(
    ids = compound_2_cids$cids,
    properties = c("Title", "MolecularFormula", "InChIKey", "MolecularWeight")
)

annotatePubchemCompound(
    cids = compound_2_cids$cids,, 
    heading = "CAS"
)
```
