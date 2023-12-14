# AnnotationGx
  <!-- badges: start -->
  [![R-CMD-check](https://github.com/bhklab/AnnotationGx/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bhklab/AnnotationGx/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->
An R package to query various bio/chem-informatics databases APIs to construct annotation files.

## Installing AnnotationGx

To install AnnotationGx, run the following:
```r
remotes::install_github("bhklab/AnnotationGx", build_manual=TRUE, build_vignettes=TRUE)
```

If you want to use the development version, run:
```r
remotes::install_github("bhklab/AnnotationGx@development", build_manual=TRUE, build_vignettes=TRUE)
```

## Package Documentation

To view an index of package documentation, run:
```r
help(package="AnnotationGx")
```

Available vignettes can be listed via;
```r
vignette(package="AnnotationGx")
```

To open a vignette in the browser, run:
```r
vignette("UniChemAPIQueries")  # replace the argument with short vignette name
```
