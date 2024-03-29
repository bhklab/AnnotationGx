# Build with: docker build -t bhklab/annotationgx-r:$TAG -f Dockerfile .
#  you can set TAG=$(grep  'Version:' DESCRIPTION  | grep -oE '[0-9]+(\.[0-9]+)*')
# To push to dockerhub run docker push bhklab/annotationgx-r:$TAG
FROM bioconductor/bioconductor:3.18

# copy the current directory contents into the container at /app
COPY . /app

# set a working directory
WORKDIR /app

# RUN R -e 'install.packages(c("BiocManager", "devtools", "jsonlite", "qpdf"), repos=c("https://cloud.r-project.org/", "https://cran.rstudio.com/"))'
RUN R -e 'install.packages("pak", repos = sprintf("https://r-lib.github.io/p/pak/stable/%s/%s/%s", .Platform$pkgType, R.Version()$os, R.Version()$arch))'
RUN R -e 'pak::pkg_install(".", dependencies=TRUE, upgrade=TRUE, ask = FALSE)'
RUN R -e 'pak::cache_clean(); pak::meta_clean(force = TRUE)'

# RUN install2.r --error --deps TRUE \
#     qpdf \
#     devtools \
#     jsonlite \
#     && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# RUN apt-get clean && apt-get -y update && apt-get install -y --no-install-recommends curl

# RUN installGithub.r \
#     bhklab/AnnotationGx