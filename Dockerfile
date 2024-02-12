# Build with: docker build -t jjjermiah/annotationgx-r:$TAG -f Dockerfile .
#  you can set TAG=$(grep  'Version:' DESCRIPTION  | grep -oE '[0-9]+(\.[0-9]+)*')
FROM rocker/r-base

RUN apt-get -qq update && \
  apt-get install -y --no-install-recommends git libxml2-dev

COPY DESCRIPTION .

RUN Rscript -e '                           \
  install.packages("remotes");             \
  remotes::install_deps(dependencies = c(  \
    "Imports",                             \
    "LinkingTo",                           \
    "Config/Needs/development"             \
  ))                                       \
'