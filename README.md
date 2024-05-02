
# AnnotationGx

<!-- badges: start -->
![GitHub R package version](https://img.shields.io/github/r-package/v/bhklab/AnnotationGx)
![GitHub R package version (development)](https://img.shields.io/github/r-package/v/bhklab/AnnotationGx/development)
![CRAN/METACRAN Version](https://img.shields.io/cran/v/AnnotationGx?label=CRAN%20RELEASE%20COMING%20SOON!&labelColor=red&color=red)

[![R-CMD-check](https://github.com/bhklab/AnnotationGx/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bhklab/AnnotationGx/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/github/bhklab/AnnotationGx/graph/badge.svg?token=Nb1x0FcJoi)](https://codecov.io/github/bhklab/AnnotationGx)
[![Docker Pulls](https://img.shields.io/docker/pulls/bhklab/annotationgx-r)](https://hub.docker.com/r/bhklab/annotationgx-r)


<!-- badges: end -->

## Installation

You can install from github using:

``` r
remotes::install_github(
    "bhklab/AnnotationGx", 
    build_manual=TRUE, 
    build_vignettes=TRUE
)
```

If you are having trouble installing the package, you can try to install without building the manual and vignettes:

``` r
remotes::install_github(
    "bhklab/AnnotationGx", 
    build_manual=FALSE, 
    build_vignettes=FALSE
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