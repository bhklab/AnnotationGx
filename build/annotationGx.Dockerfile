# This file is for building the annotationGx image for linux

FROM bioconductor/bioconductor_docker:3.17-R-4.3.0

# Install required libraries -- using prebuild binaries where available
RUN apt-get update && apt-get install -y \
    git
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

# install data.table 
# RUN R -e "BiocManager::install('data.table', update = FALSE, ask = FALSE)"

RUN install2.r --error --deps TRUE pak
# add "https://r.acidgenomics.com", BiocManager::repositories() to the list of repositories

RUN R -e "install.packages( \
        pkgs = 'Cellosaurus',\
        repos = c(\
            'https://r.acidgenomics.com',\
            BiocManager::repositories()\
        ),\
        dependencies = TRUE\
    )"


RUN R -e "pak::pkg_install('bhklab/AnnotationGx@main', ask = F)"

RUN R -e "pak::pak_cleanup(force=TRUE)"
RUN rm -rf /tmp/*

CMD ["R"]