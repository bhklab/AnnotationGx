# Introduction to AnnotationGx

## Basics

### Install `AnnotationGx`

`R` is an open-source statistical environment which can be easily
modified to enhance its functionality via packages. `AnnotationGx` is a
`R` package

TODO::after submitting to cran update this

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
#>   0.0.0.9097, <https://bhklab.github.io/AnnotationGx/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {AnnotationGx: AnnotationGx: A package for building, updating and querying an
#> annotation database for pharmaco-genomic data},
#>     author = {Michael Tran and Jermiah Joseph and Christopher Eeles and Benjamin Haibe-Kains},
#>     year = {2026},
#>     note = {R package version 0.0.0.9097},
#>     url = {https://bhklab.github.io/AnnotationGx/},
#>   }
```

## Quick start to using `AnnotationGx`

``` r
library("AnnotationGx")
```

    #> ─ Session info ───────────────────────────────────────────────────────────────────────────────────────────────────────
    #>  setting  value
    #>  version  R version 4.5.2 (2025-10-31)
    #>  os       Ubuntu 24.04.3 LTS
    #>  system   x86_64, linux-gnu
    #>  ui       X11
    #>  language en
    #>  collate  C.UTF-8
    #>  ctype    C.UTF-8
    #>  tz       UTC
    #>  date     2026-02-11
    #>  pandoc   3.1.11 @ /opt/hostedtoolcache/pandoc/3.1.11/x64/ (via rmarkdown)
    #>  quarto   NA
    #> 
    #> ─ Packages ───────────────────────────────────────────────────────────────────────────────────────────────────────────
    #>  package      * version    date (UTC) lib source
    #>  AnnotationGx * 0.0.0.9097 2026-02-11 [1] local
    #>  backports      1.5.0      2024-05-23 [1] RSPM
    #>  bslib          0.10.0     2026-01-26 [1] RSPM
    #>  cachem         1.1.0      2024-05-16 [1] RSPM
    #>  checkmate      2.3.4      2026-02-03 [1] RSPM
    #>  cli            3.6.5      2025-04-23 [1] RSPM
    #>  data.table     1.18.2.1   2026-01-27 [1] RSPM
    #>  desc           1.4.3      2023-12-10 [1] RSPM
    #>  digest         0.6.39     2025-11-19 [1] RSPM
    #>  evaluate       1.0.5      2025-08-27 [1] RSPM
    #>  fastmap        1.2.0      2024-05-15 [1] RSPM
    #>  fs             1.6.6      2025-04-12 [1] RSPM
    #>  htmltools      0.5.9      2025-12-04 [1] RSPM
    #>  jquerylib      0.1.4      2021-04-26 [1] RSPM
    #>  jsonlite       2.0.0      2025-03-27 [1] RSPM
    #>  knitr          1.51       2025-12-20 [1] RSPM
    #>  lifecycle      1.0.5      2026-01-08 [1] RSPM
    #>  pkgdown        2.2.0      2025-11-06 [1] any (@2.2.0)
    #>  R6             2.6.1      2025-02-15 [1] RSPM
    #>  ragg           1.5.0      2025-09-02 [1] RSPM
    #>  rlang          1.1.7      2026-01-09 [1] RSPM
    #>  rmarkdown      2.30       2025-09-28 [1] RSPM
    #>  sass           0.4.10     2025-04-11 [1] RSPM
    #>  sessioninfo  * 1.2.3      2025-02-05 [1] RSPM
    #>  systemfonts    1.3.1      2025-10-01 [1] RSPM
    #>  textshaping    1.0.4      2025-10-10 [1] RSPM
    #>  xfun           0.56       2026-01-18 [1] RSPM
    #>  yaml           2.3.12     2025-12-10 [1] RSPM
    #> 
    #>  [1] /home/runner/work/_temp/Library
    #>  [2] /opt/R/4.5.2/lib/R/site-library
    #>  [3] /opt/R/4.5.2/lib/R/library
    #>  * ── Packages attached to the search path.
    #> 
    #> ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
