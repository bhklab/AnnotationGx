# This file is for building the annotationGx image for linux

FROM bioconductor/bioconductor_docker:3.17-R-4.3.0

# Install required libraries -- using prebuild binaries where available
# RUN apt-get update && apt-get install -y \
#     git \
#     r-cran-data.table \
#     r-cran-doparallel \
#     r-cran-dygraphs \
#     r-cran-foreach \
#     r-cran-fs \
#     r-cran-future.apply \
#     r-cran-gh \
#     r-cran-git2r \
#     r-cran-igraph \
#     r-cran-memoise \
#     r-cran-png \
#     r-cran-rcpparmadillo \
#     r-cran-rex \
#     r-cran-runit \
#     r-cran-stringdist \
#     r-cran-testthat \
#     r-cran-tidyverse \
#     r-cran-tinytest \
#     r-cran-xts \
#     sqlite3 \
#     sudo

# Install R packages
# RUN install2.r --error --deps TRUE BiocManager



RUN rm -rf /tmp/*

CMD ["R"]