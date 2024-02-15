# Build with: docker build -t jjjermiah/annotationgx-r:$TAG -f Dockerfile .
#  you can set TAG=$(grep  'Version:' DESCRIPTION  | grep -oE '[0-9]+(\.[0-9]+)*')
# To push to dockerhub run docker push jjjermiah/annotationgx-r:$TAG
FROM rocker/r-base

RUN apt-get -qq update && \
  apt-get install -y --no-install-recommends git libxml2-dev

RUN Rscript -e "install.packages('pak', repos = 'https://r-lib.github.io/p/pak/dev/')"
RUN Rscript -e "pak::pkg_install(c('jsonlite','jjjermiah/AnnotationGx'), ask = F)"

