# Build with: docker build -t bhklab/annotationgx-r:$TAG -f Dockerfile .
#  you can set TAG=$(grep  'Version:' DESCRIPTION  | grep -oE '[0-9]+(\.[0-9]+)*')
# To push to dockerhub run docker push bhklab/annotationgx-r:$TAG
FROM rocker/r-base

# copy the current directory contents into the container at /app
COPY . /app

# set a working directory
WORKDIR /app

RUN apt-get -qq update && \
  apt-get install -y --no-install-recommends git libxml2-dev

RUN Rscript -e "install.packages('pak', repos = 'https://r-lib.github.io/p/pak/dev/')"
RUN Rscript -e "pak::pkg_install(c('jsonlite','bhklab/AnnotationGx@main'), ask = F)"


