# Introduction to AnnotationGx

## Basics

### Install `AnnotationGx`

`R` is an open-source statistical environment which can be easily
modified to enhance its functionality via packages. `AnnotationGx` is a
`R` package hosted on Bioconductor and can be installed using
`BiocManager`.

### Citing `AnnotationGx`

We hope that `AnnotationGx` will be useful for your research. Please use
the following information to cite the package and the overall approach.
Thank you!

``` r
## Citation info
citation("AnnotationGx")
#> To cite package 'AnnotationGx' in publications use:
#> 
#>   Tran M, Joseph J, Eeles C, Haibe-Kains B (2026). _AnnotationGx:
#>   AnnotationGx: A package for building, updating and querying an
#>   annotation database for pharmaco-genomic data_. R package version
#>   0.99.3, <https://bhklab.github.io/AnnotationGx/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {AnnotationGx: AnnotationGx: A package for building, updating and querying an
#> annotation database for pharmaco-genomic data},
#>     author = {Michael Tran and Jermiah Joseph and Christopher Eeles and Benjamin Haibe-Kains},
#>     year = {2026},
#>     note = {R package version 0.99.3},
#>     url = {https://bhklab.github.io/AnnotationGx/},
#>   }
```

## Quick start to using `AnnotationGx`

``` r
library("AnnotationGx")
```

``` r
sessionInfo()
#> R version 4.5.3 (2026-03-11)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.4 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] AnnotationGx_0.99.3
#> 
#> loaded via a namespace (and not attached):
#>  [1] backports_1.5.1     digest_0.6.39       desc_1.4.3         
#>  [4] R6_2.6.1            fastmap_1.2.0       xfun_0.57          
#>  [7] cachem_1.1.0        knitr_1.51          memoise_2.0.1      
#> [10] htmltools_0.5.9     rmarkdown_2.31      lifecycle_1.0.5    
#> [13] cli_3.6.6           sass_0.4.10         pkgdown_2.2.0      
#> [16] data.table_1.18.2.1 textshaping_1.0.5   jquerylib_0.1.4    
#> [19] systemfonts_1.3.2   compiler_4.5.3      tools_4.5.3        
#> [22] ragg_1.5.2          checkmate_2.3.4     evaluate_1.0.5     
#> [25] bslib_0.10.0        yaml_2.3.12         jsonlite_2.0.0     
#> [28] rlang_1.2.0         fs_2.1.0
```
